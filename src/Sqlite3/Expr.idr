module Sqlite3.Expr

import Data.Bits
import Data.Buffer.Indexed
import Data.ByteString
import Data.String

import Sqlite3.Marshall
import Sqlite3.Table
import Sqlite3.Types

%default total

public export
data Numeric : SqliteType -> Type where
  N_INTEGER : Numeric INTEGER
  N_REAL    : Numeric REAL

||| A syntax tree type representing well-typed SQLite expressions.
public export
data Expr : Schema -> SqliteType -> Type where
  ||| A literal value in an SQL expression
  Lit    : (t : SqliteType) -> (v : IdrisType t) -> Expr s t

  ||| The `NULL` literal
  NULL   : Expr s t

  ||| Alias for the literal `1`, which corresponds to `True` in
  ||| boolean expressions
  TRUE   : Expr s BOOL

  ||| Alias for the literal `0`, which corresponds to `False` in
  ||| boolean expressions
  FALSE  : Expr s BOOL

  ||| A raw expressions string that will be used as given.
  Raw    : String -> Expr s t

  ||| A column in a list of tables. Typically, it is convenient to
  ||| use string literals directly for this (see `Expr.fromString`).
  Col    :
       {0 s      : Schema}
    -> (col      : String)
    -> {auto 0 p : IsJust (FindSchemaCol col s)}
    -> Expr s (SchemaColType col s)

  ||| Greater-than operator.
  |||
  ||| Note, that this is subject to SQL's three-valued logic in the
  ||| presence of `NULL`.
  (>)    : Expr s t -> Expr s t -> Expr s BOOL

  ||| Less-than operator.
  |||
  ||| Note, that this is subject to SQL's three-valued logic in the
  ||| presence of `NULL`.
  (<)    : Expr s t -> Expr s t -> Expr s BOOL

  ||| Greater-than or equals operator.
  |||
  ||| Note, that this is subject to SQL's three-valued logic in the
  ||| presence of `NULL`.
  (>=)   : Expr s t -> Expr s t -> Expr s BOOL

  ||| Less-than or equals operator.
  |||
  ||| Note, that this is subject to SQL's three-valued logic in the
  ||| presence of `NULL`.
  (<=)   : Expr s t -> Expr s t -> Expr s BOOL

  ||| Equality operator.
  |||
  ||| Note, that this is subject to SQL's three-valued logic in the
  ||| presence of `NULL`.
  (==)   : Expr s t -> Expr s t -> Expr s BOOL

  ||| Inequality operator.
  |||
  ||| We use the same operator as in the `Eq` interface here, but this
  ||| corresponds to SQL's `<>` (or `!=`) operator.
  |||
  ||| Note, that this is subject to SQL's three-valued logic in the
  ||| presence of `NULL`.
  (/=)   : Expr s t -> Expr s t -> Expr s BOOL

  ||| Equality test.
  |||
  ||| Unlike `(==)`, this will always result in `TRUE` or `FALSE`
  ||| even in the presence of `NULL`.
  IS     : Expr s t -> Expr s t -> Expr s BOOL

  ||| Inequality test.
  |||
  ||| Unlike `(==)`, this will always result in `TRUE` or `FALSE`
  ||| even in the presence of `NULL`.
  IS_NOT : Expr s t -> Expr s t -> Expr s BOOL

  ||| Logical `AND`.
  |||
  ||| Note, that this is subject to SQL's three-valued logic in the
  ||| presence of `NULL`.
  (&&)   : Expr s BOOL -> Expr s BOOL -> Expr s BOOL

  ||| Logical `OR`.
  |||
  ||| Note, that this is subject to SQL's three-valued logic in the
  ||| presence of `NULL`.
  (||)   : Expr s BOOL -> Expr s BOOL -> Expr s BOOL

  ||| Logical negation.
  |||
  ||| Note, that this is subject to SQL's three-valued logic in the
  ||| presence of `NULL`.
  NOT    : Expr s BOOL -> Expr s BOOL

  ||| Bit-wise `AND`.
  |||
  ||| This corresponds to SQLite's `&` operator.
  (.&.)  : Expr s INTEGER -> Expr s INTEGER -> Expr s INTEGER

  ||| Bit-wise `OR`.
  |||
  ||| This corresponds to SQLite's `|` operator.
  (.|.)  : Expr s INTEGER -> Expr s INTEGER -> Expr s INTEGER

  ||| Bit-wise left shift.
  |||
  ||| This corresponds to SQLite's `<<` operator.
  ShiftL : Expr s INTEGER -> Expr s INTEGER -> Expr s INTEGER

  ||| Bit-wise right shift.
  |||
  ||| This corresponds to SQLite's `>>` operator.
  ShiftR : Expr s INTEGER -> Expr s INTEGER -> Expr s INTEGER


  ||| Numeric addition.
  |||
  ||| Since `Expr s t` implements `Num` for numeric types `t`, you can
  ||| typically use the addition operator `(+)` instead of this constructor.
  Add   : (0 prf : Numeric t) => Expr s t -> Expr s t -> Expr s t

  ||| Numeric multiplication.
  |||
  ||| Since `Expr s t` implements `Num` for numeric types `t`, you can
  ||| typically use the multiplication operator `(*)` instead of this
  ||| constructor.
  Mult  : (0 prf : Numeric t) => Expr s t -> Expr s t -> Expr s t

  ||| Numeric subtraction.
  |||
  ||| Since `Expr s t` implements `Neg` for numeric types `t`, you can
  ||| typically use the subtraction operator `(-)` instead of this
  ||| constructor.
  Sub   : (0 prf : Numeric t) => Expr s t -> Expr s t -> Expr s t

  ||| Computes the absolute of an expressions.
  |||
  ||| This corresponds to the `abs()` function.
  Abs   : (0 prf : Numeric t) => Expr s t -> Expr s t

  ||| Numeric negation.
  Neg   : (0 prf : Numeric t) => Expr s t -> Expr s t

  ||| Numeric division.
  |||
  ||| Since `Expr s t` implements `Integral` for numeric types `INTEGER`,
  ||| you can typically use `div` for integer division. Likewise, you can
  ||| use `(/)` for floating point division.
  Div   : (0 prf : Numeric t) => Expr s t -> Expr s t -> Expr s t

  ||| Computes the modulus of two integers.
  |||
  ||| This corresponds to the `%` operator in SQL.
  Mod   : Expr s INTEGER -> Expr s INTEGER -> Expr s INTEGER

  ||| String concatenation.
  |||
  ||| This corresponds to the `||` operator in SQL.
  (++)   : Expr s TEXT -> Expr s TEXT -> Expr s TEXT

  ||| The current time as a string.
  CURRENT_TIME      : Expr s TEXT

  ||| The current date as a string.
  CURRENT_DATE      : Expr s TEXT

  ||| The current date and time as a string.
  CURRENT_TIMESTAMP : Expr s TEXT

  ||| Matches the given text expression against the given
  ||| text pattern.
  LIKE              : Expr s TEXT -> (pattern : Expr s TEXT) -> Expr s BOOL

  ||| Matches the given text expression against the given
  ||| GLOB pattern.
  GLOB              : Expr s TEXT -> (pattern : Expr s TEXT) -> Expr s BOOL

  ||| True, if the given value appears in the given list of values.
  IN                : Expr s t -> List (Expr s t) -> Expr s BOOL

  ||| Returns the first non-NULL value in the given list of expressions.
  COALESCE          : List (Expr s t) -> Expr s t

  ||| Counts the number of aggregated values.
  |||
  ||| This is typically used with a `GROUP BY` statement.
  COUNT             : Expr s t -> Expr s INTEGER

  ||| Returns the average of accumulated values.
  |||
  ||| This is typically used with a `GROUP BY` statement.
  AVG               : (0 prf : Numeric t) => Expr s t -> Expr s REAL

  ||| Returns the sum of accumulated values.
  |||
  ||| This is typically used with a `GROUP BY` statement.
  SUM               : (0 prf : Numeric t) => Expr s t -> Expr s t

  ||| Returns the minimum of accumulated values.
  |||
  ||| This is typically used with a `GROUP BY` statement.
  MIN               : Expr s t -> Expr s t

  ||| Returns the maximum of accumulated values.
  |||
  ||| This is typically used with a `GROUP BY` statement.
  MAX               : Expr s t -> Expr s t

  ||| Concatenates aggregated text values using the given separator.
  |||
  ||| This is typically used with a `GROUP BY` statement.
  GROUP_CONCAT      : Expr s TEXT -> (sep : String) -> Expr s TEXT

export %inline
Num (Expr s INTEGER) where
  fromInteger = Lit INTEGER . fromInteger
  (+) = Add
  (*) = Mult

export %inline
Neg (Expr s INTEGER) where
  negate = Neg
  (-)    = Sub

export %inline
Integral (Expr s INTEGER) where
  div = Div
  mod = Mod

export %inline
Num (Expr s REAL) where
  fromInteger = Lit REAL . fromInteger
  (+) = Add
  (*) = Mult

export %inline
Neg (Expr s REAL) where
  negate = Neg
  (-)    = Sub

export %inline
Fractional (Expr s REAL) where
  (/) = Div

export %inline
FromDouble (Expr s REAL) where
  fromDouble = Lit REAL

export %inline
fromString :
     {s        : Schema}
  -> (col      : String)
  -> {auto 0 p : IsJust (FindSchemaCol col s)}
  -> Expr s (SchemaColType col s)
fromString = Col

||| Convert a value of a marshallable type to a literal expression.
export
val : ToCell a => a -> Expr s (ToCellType a)
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

||| Converts a list of values to strings and concatenates them using
||| a comma as the separator.
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

encFun1 : String -> Expr s t -> String

encFun : String -> List (Expr s t) -> String

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
encodeExpr (Add x y)    = encOp "+" x y
encodeExpr (Mult x y)   = encOp "*" x y
encodeExpr (Sub x y)    = encOp "-" x y
encodeExpr (Div x y)    = encOp "/" x y
encodeExpr (Mod x y)    = encOp "%" x y
encodeExpr (Abs y)      = encFun1 "abs" y
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
encodeExpr (Neg x)      = encPrefix "-" x
encodeExpr (Raw s)      = s
encodeExpr (Col c)      = c
encodeExpr NULL         = "NULL"
encodeExpr TRUE         = "1"
encodeExpr FALSE        = "0"
encodeExpr CURRENT_TIME      = "CURRENT_TIME"
encodeExpr CURRENT_DATE      = "CURRENT_DATE"
encodeExpr CURRENT_TIMESTAMP = "CURRENT_TIMESTAMP"
encodeExpr (LIKE x y)        = encOp "LIKE" x y
encodeExpr (GLOB x y)        = encOp "GLOB" x y
encodeExpr (IN x xs)         = "\{encodeExpr x} IN (\{encExprs [<] xs})"
encodeExpr (COALESCE xs)     = encFun "coalesce" xs
encodeExpr (COUNT x)         = encFun1 "count" x
encodeExpr (AVG x)           = encFun1 "avg" x
encodeExpr (SUM x)           = encFun1 "sum" x
encodeExpr (MIN x)           = encFun1 "min" x
encodeExpr (MAX x)           = encFun1 "max" x
encodeExpr (GROUP_CONCAT x s) = "group_concat(\{encodeExpr x}, \{s})"

encOp s x y =
  let sx := encodeExpr x
      sy := encodeExpr y
   in "(\{sx} \{s} \{sy})"

encPrefix s x = "\{s}(\{encodeExpr x})"

encExprs sc []      = commaSep id (sc <>> [])
encExprs sc (x::xs) = encExprs (sc :< encodeExpr x) xs

encFun f xs      = "\{f}(\{encExprs [<] xs})"

encFun1 f x      = "\{f}(\{encodeExpr x})"
