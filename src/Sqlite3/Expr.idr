module Sqlite3.Expr

import Data.Bits
import Data.Buffer.Indexed
import Data.ByteString
import Data.String

import Sqlite3.Marshall
import Sqlite3.Table
import Sqlite3.Types

%default total

||| Type representing well-typed SQLite expressions.
public export
data Expr : Schema -> SqliteType -> Type where
  Lit    : (t : SqliteType) -> (v : IdrisType t) -> Expr s t
  NULL   : Expr s t
  TRUE   : Expr s BOOL
  FALSE  : Expr s BOOL
  Raw    : String -> Expr s t
  Col    :
       (tbl,col  : String)
    -> {auto 0 p : HasCol s tbl col}
    -> Expr s (ColType s p)

  C      :
       (col      : String)
    -> {auto 0 p : TableHasCol t col}
    -> Expr [t] (ListColType t.cols p)

  (>)    : Expr s t -> Expr s t -> Expr s BOOL
  (<)    : Expr s t -> Expr s t -> Expr s BOOL
  (>=)   : Expr s t -> Expr s t -> Expr s BOOL
  (<=)   : Expr s t -> Expr s t -> Expr s BOOL
  (==)   : Expr s t -> Expr s t -> Expr s BOOL
  (/=)   : Expr s t -> Expr s t -> Expr s BOOL
  IS     : Expr s t -> Expr s t -> Expr s BOOL
  IS_NOT : Expr s t -> Expr s t -> Expr s BOOL
  (&&)   : Expr s BOOL -> Expr s BOOL -> Expr s BOOL
  (||)   : Expr s BOOL -> Expr s BOOL -> Expr s BOOL
  NOT    : Expr s BOOL -> Expr s BOOL
  (.&.)  : Expr s INTEGER -> Expr s INTEGER -> Expr s INTEGER
  (.|.)  : Expr s INTEGER -> Expr s INTEGER -> Expr s INTEGER
  ShiftL : Expr s INTEGER -> Expr s INTEGER -> Expr s INTEGER
  ShiftR : Expr s INTEGER -> Expr s INTEGER -> Expr s INTEGER

  AddI   : Expr s INTEGER -> Expr s INTEGER -> Expr s INTEGER
  MultI  : Expr s INTEGER -> Expr s INTEGER -> Expr s INTEGER
  SubI   : Expr s INTEGER -> Expr s INTEGER -> Expr s INTEGER
  NegI   : Expr s INTEGER -> Expr s INTEGER
  DivI   : Expr s INTEGER -> Expr s INTEGER -> Expr s INTEGER
  Mod    : Expr s INTEGER -> Expr s INTEGER -> Expr s INTEGER

  AddD   : Expr s REAL -> Expr s REAL -> Expr s REAL
  MultD  : Expr s REAL -> Expr s REAL -> Expr s REAL
  SubD   : Expr s REAL -> Expr s REAL -> Expr s REAL
  NegD   : Expr s REAL -> Expr s REAL
  DivD   : Expr s REAL -> Expr s REAL -> Expr s REAL

  (++)   : Expr s TEXT -> Expr s TEXT -> Expr s TEXT

  CURRENT_TIME      : Expr s TEXT
  CURRENT_DATE      : Expr s TEXT
  CURRENT_TIMESTAMP : Expr s TEXT
  LIKE              : Expr s TEXT -> Expr s TEXT -> Expr s BOOL
  NOT_LIKE          : Expr s TEXT -> Expr s TEXT -> Expr s BOOL
  GLOB              : Expr s TEXT -> Expr s TEXT -> Expr s BOOL
  NOT_GLOB          : Expr s TEXT -> Expr s TEXT -> Expr s BOOL
  IN                : Expr s t -> List (Expr s t) -> Expr s BOOL
  NOT_IN            : Expr s t -> List (Expr s t) -> Expr s BOOL

export %inline
Num (Expr s INTEGER) where
  fromInteger = Lit INTEGER . fromInteger
  (+) = AddI
  (*) = MultI

export %inline
Neg (Expr s INTEGER) where
  negate = NegI
  (-)    = SubI

export %inline
Integral (Expr s INTEGER) where
  div = DivI
  mod = Mod

export %inline
Num (Expr s REAL) where
  fromInteger = Lit REAL . fromInteger
  (+) = AddD
  (*) = MultD

export %inline
Neg (Expr s REAL) where
  negate = NegD
  (-)    = SubD

export %inline
Fractional (Expr s REAL) where
  (/) = DivD

export %inline
FromDouble (Expr s REAL) where
  fromDouble = Lit REAL

export %inline
fromString :
     (col      : String)
  -> {auto 0 p : TableHasCol t col}
  -> Expr [t] (ListColType t.cols p)
fromString = C

||| Convert a value of a marshallable type to a literal expression.
export
val : AsCell a => a -> Expr s (CellType a)
val x =
  case toCell x of
    Nothing => NULL
    Just v  => Lit _ v

export %inline
text : String -> Expr s TEXT
text = val

export %inline
int : Int64 -> Expr s INTEGER
int = val

export %inline
blob : ByteString -> Expr s BLOB
blob = val

export %inline
real : Double -> Expr s REAL
real = val

--------------------------------------------------------------------------------
-- Encode
--------------------------------------------------------------------------------

export
commaSep : (a -> String) -> List a -> String
commaSep f = concat . intersperse ", " . map f

hexChar : Bits8 -> Char
hexChar 0 = '0'
hexChar 1 = '1'
hexChar 2 = '2'
hexChar 3 = '3'
hexChar 4 = '4'
hexChar 5 = '5'
hexChar 6 = '6'
hexChar 7 = '7'
hexChar 8 = '8'
hexChar 9 = '9'
hexChar 10 = 'a'
hexChar 11 = 'b'
hexChar 12 = 'c'
hexChar 13 = 'd'
hexChar 14 = 'e'
hexChar _  = 'f'

%inline quote : Char
quote = '\''

||| Encodes a `ByteString` as an SQL literal.
|||
||| Every byte is encodec with two hexadecimal digits, and the
||| whole string is wrapped in single quotes prefixed with an "X".
|||
||| For instance, `encodeBytes (fromList [0xa1, 0x77])` yields the
||| string "X'a177'".
export
encodeBytes : ByteString -> String
encodeBytes = pack . (\x => 'X'::quote::x) . foldr acc [quote]
  where
    %inline acc : Bits8 -> List Char -> List Char
    acc b cs = hexChar (b `shiftR` 4) :: hexChar (b .&. 0xf) :: cs

||| Encodes a `String` as an SQL literal.
|||
||| The whole string is wrapped in single quotes. Single quotes withing
||| the string are escaped by doubling them.
export
encodeText : String -> String
encodeText = go [<quote] . unpack
  where
    go : SnocList Char -> List Char -> String
    go sc []           = pack $ sc <>> [quote]
    go sc ('\'' :: xs) = go (sc :< quote :< quote) xs
    go sc (x    :: xs) = go (sc :< x) xs

encBool : Bool -> String
encBool True  = "1"
encBool False = "0"

||| Encodes an SQL literal as a string.
export
encodeLit : (t : SqliteType) -> IdrisType t -> String
encodeLit BLOB x    = encodeBytes x
encodeLit TEXT x    = encodeText x
encodeLit INTEGER x = show x
encodeLit REAL x    = show x
encodeLit BOOL x    = encBool x

encOp : String -> Expr s t -> Expr s t -> String

encPrefix : String -> Expr s t -> String

encExprs : SnocList String -> List (Expr s t) -> String

||| Encodes an expression as a string.
|||
||| Literals will be correctly escaped and converted.
|||
||| Note: See module `Sqlite3.Parameter` for encoding of expressions
|||       with SQL parameters that will be bound separately when
|||       binding the statement.
export
encodeExpr : Expr s t -> String
encodeExpr (Lit t v)    = encodeLit t v
encodeExpr (AddI x y)   = encOp "+" x y
encodeExpr (MultI x y)  = encOp "*" x y
encodeExpr (SubI x y)   = encOp "-" x y
encodeExpr (DivI x y)   = encOp "/" x y
encodeExpr (Mod x y)    = encOp "%" x y
encodeExpr (AddD x y)   = encOp "+" x y
encodeExpr (MultD x y)  = encOp "*" x y
encodeExpr (SubD x y)   = encOp "-" x y
encodeExpr (DivD x y)   = encOp "/" x y
encodeExpr (x < y)      = encOp "<" x y
encodeExpr (x > y)      = encOp ">" x y
encodeExpr (x <= y)     = encOp "<=" x y
encodeExpr (x >= y)     = encOp ">=" x y
encodeExpr (x == y)     = encOp "==" x y
encodeExpr (IS x y)     = encOp "IS" x y
encodeExpr (IS_NOT x y) = encOp "IS NOT" x y
encodeExpr (x /= y)     = encOp "!=" x y
encodeExpr (x && y)     = encOp "AND" x y
encodeExpr (x || y)     = encOp "OR" x y
encodeExpr (x ++ y)     = encOp "||" x y
encodeExpr (x .&. y)    = encOp "&" x y
encodeExpr (x .|. y)    = encOp "|" x y
encodeExpr (ShiftR x y) = encOp ">>" x y
encodeExpr (ShiftL x y) = encOp "<<" x y
encodeExpr (NOT x)      = encPrefix "NOT" x
encodeExpr (NegI x)     = encPrefix "-" x
encodeExpr (NegD x)     = encPrefix "-" x
encodeExpr (Raw s)      = s
encodeExpr (Col t c)    = "\{t}.\{c}"
encodeExpr (C c)        = c
encodeExpr NULL         = "NULL"
encodeExpr TRUE         = "1"
encodeExpr FALSE        = "0"
encodeExpr CURRENT_TIME      = "CURRENT_TIME"
encodeExpr CURRENT_DATE      = "CURRENT_DATE"
encodeExpr CURRENT_TIMESTAMP = "CURRENT_TIMESTAMP"
encodeExpr (LIKE x y)        = encOp "LIKE" x y
encodeExpr (NOT_LIKE x y)    = encOp "NOT_LIKE" x y
encodeExpr (GLOB x y)        = encOp "GLOB" x y
encodeExpr (NOT_GLOB x y)    = encOp "NOT_GLOB" x y
encodeExpr (IN x xs)         = "\{encodeExpr x} IN (\{encExprs [<] xs})"
encodeExpr (NOT_IN x xs)     = "\{encodeExpr x} NOT IN (\{encExprs [<] xs})"

encOp s x y =
  let sx := encodeExpr x
      sy := encodeExpr y
   in "(\{sx} \{s} \{sy})"

encPrefix s x = "\{s}(\{encodeExpr x})"

encExprs sc []      = commaSep id (sc <>> [])
encExprs sc (x::xs) = encExprs (sc :< encodeExpr x) xs
