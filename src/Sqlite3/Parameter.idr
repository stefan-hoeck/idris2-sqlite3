module Sqlite3.Parameter

import public Control.Monad.State

import Data.Buffer.Indexed
import Data.ByteString
import Data.List.Quantifiers
import Data.Maybe
import Data.SortedMap
import Data.String

import Sqlite3.Cmd
import Sqlite3.Expr
import Sqlite3.Marshall
import Sqlite3.Table
import Sqlite3.Types

%default total

||| Parameter to be bound in an SQL statement.
public export
record Parameter where
  constructor P
  name  : String
  type  : SqliteType
  value : IdrisType type

||| State type used to keep track of the parameters used in an
||| SQLite statement that is being assembled.
public export
record ParamST where
  constructor PS
  ix   : Nat
  args : List Parameter

||| Initial list of parameters
export
init : ParamST
init = PS 0 []

||| Utility alias for an SQL statement with parameters.
public export
0 ParamStmt : Type
ParamStmt = State ParamST String

--------------------------------------------------------------------------------
-- Encode with parameters
--------------------------------------------------------------------------------

encOp : String -> Expr s t -> Expr s t -> ParamStmt

encPrefix : String -> Expr s t -> ParamStmt

encExprs : SnocList String -> List (Expr s t) -> ParamStmt

||| Encodes an expression, generating a list of parameters with
||| unique names that will be bound when running the SQL statement.
export
encodeExprP : Expr s t -> ParamStmt
encodeExprP (Lit t v)    =
  state $ \(PS x as) =>
    let s := ":\{show x}"
     in (PS (S x) (P s t v :: as), s)

encodeExprP (AddI x y)   = encOp "+" x y
encodeExprP (MultI x y)  = encOp "*" x y
encodeExprP (SubI x y)   = encOp "-" x y
encodeExprP (DivI x y)   = encOp "/" x y
encodeExprP (Mod x y)    = encOp "%" x y
encodeExprP (AddD x y)   = encOp "+" x y
encodeExprP (MultD x y)  = encOp "*" x y
encodeExprP (SubD x y)   = encOp "-" x y
encodeExprP (DivD x y)   = encOp "/" x y
encodeExprP (x < y)      = encOp "<" x y
encodeExprP (x > y)      = encOp ">" x y
encodeExprP (x <= y)     = encOp "<=" x y
encodeExprP (x >= y)     = encOp ">=" x y
encodeExprP (x == y)     = encOp "==" x y
encodeExprP (x /= y)     = encOp "!=" x y
encodeExprP (IS x y)     = encOp "IS" x y
encodeExprP (IS_NOT x y) = encOp "IS NOT" x y
encodeExprP (x && y)     = encOp "AND" x y
encodeExprP (x || y)     = encOp "OR" x y
encodeExprP (x ++ y)     = encOp "||" x y
encodeExprP (x .&. y)    = encOp "&" x y
encodeExprP (x .|. y)    = encOp "|" x y
encodeExprP (ShiftR x y) = encOp ">>" x y
encodeExprP (ShiftL x y) = encOp "<<" x y
encodeExprP (NegI x)     = encPrefix "-" x
encodeExprP (NegD x)     = encPrefix "-" x
encodeExprP (NOT x)      = encPrefix "NOT" x
encodeExprP (Raw s)      = pure s
encodeExprP NULL         = pure "NULL"
encodeExprP TRUE         = pure "1"
encodeExprP FALSE        = pure "0"
encodeExprP (Col t c)    = pure "\{t}.\{c}"
encodeExprP (C c)        = pure c

encodeExprP CURRENT_TIME      = pure "CURRENT_TIME"
encodeExprP CURRENT_DATE      = pure "CURRENT_DATE"
encodeExprP CURRENT_TIMESTAMP = pure "CURRENT_TIMESTAMP"
encodeExprP (LIKE x y)        = encOp "LIKE" x y
encodeExprP (NOT_LIKE x y)    = encOp "NOT_LIKE" x y
encodeExprP (GLOB x y)        = encOp "GLOB" x y
encodeExprP (NOT_GLOB x y)    = encOp "NOT_GLOB" x y

encodeExprP (IN x xs) = do
  s  <- encodeExprP x
  ss <- encExprs [<] xs
  pure "(\{s}) IN (\{ss})"

encodeExprP (NOT_IN x xs) = do
  s  <- encodeExprP x
  ss <- encExprs [<] xs
  pure "(\{s}) NOT IN (\{ss})"

encOp s x y = do
  sx <- encodeExprP x
  sy <- encodeExprP y
  pure $ "(\{sx} \{s} \{sy})"

encPrefix s x = do
  sx <- encodeExprP x
  pure $ "\{s}(\{sx})"

encExprs sc []      = pure . commaSep id $ sc <>> []
encExprs sc (x::xs) = do
  v <- encodeExprP x
  encExprs (sc :< v) xs

--------------------------------------------------------------------------------
-- Encoding Commands
--------------------------------------------------------------------------------

record Constraints where
  constructor CS
  colConstraints : SortedMap String String
  tblConstraints : List String

addCol : Constraints -> (col, constraint : String) -> Constraints
addCol (CS cs ts) n v =
  case lookup n cs of
    Just s  => CS (insert n (s ++ " " ++ v) cs) ts
    Nothing => CS (insert n v cs) ts

addTbl : Constraints -> String -> Constraints
addTbl (CS cs ss) s = CS cs (s::ss)

names : SnocList String -> LAll (TColumn t) ts -> String
names sc []           = commaSep id (sc <>> [])
names sc (TC c :: cs) = names (sc :< c) cs

encodeDflt : Expr s t -> String
encodeDflt x         = "DEFAULT (\{encodeExpr x})"

encConstraint : Constraints -> Constraint t -> Constraints
encConstraint y (NotNull $ TC n)       = addCol y n"NOT NULL"
encConstraint y (AutoIncrement $ TC n) = addCol y n "AUTOINCREMENT"
encConstraint y (Unique [TC n])        = addCol y n "UNIQUE"
encConstraint y (PrimaryKey [TC n])    = addCol y n "PRIMARY KEY"
encConstraint y (ForeignKey [TC n])    = addCol y n "FOREIGN KEY"
encConstraint y (Default s expr)       = addCol y s (encodeDflt expr)
encConstraint y (Unique xs)            = addTbl y "UNIQUE (\{names [<] xs})"
encConstraint y (PrimaryKey xs)        = addTbl y "PRIMARY KEY (\{names [<] xs})"
encConstraint y (ForeignKey xs)        = addTbl y "FOREIGN KEY (\{names [<] xs})"
encConstraint y (Check x)              = addTbl y "CHECK (\{encodeExpr x})"

ine : Bool -> String
ine True  = "IF NOT EXISTS"
ine False = ""

ie : Bool -> String
ie True  = "IF EXISTS"
ie False = ""

encodeCols : SortedMap String String -> List Column -> List String
encodeCols m = map encodeCol
  where
    encodeCol : Column -> String
    encodeCol (C n t) =
      let constraints := fromMaybe "" (lookup n m)
       in "\{n} \{show t} \{constraints}"

insertCols : SnocList String -> Columns t ts -> String
insertCols sc []         = commaSep id (sc <>> [])
insertCols sc (TC c::cs) = insertCols (sc :< c) cs

exprs : SnocList String -> Exprs t ts -> ParamStmt
exprs sc []      = pure $ commaSep id (sc <>> [])
exprs sc (c::cs) = do
  s <- encodeExprP c
  exprs (sc :< s) cs

updateVals : SnocList String -> List (Val t) -> ParamStmt
updateVals sc []        = pure $ commaSep id (sc <>> [])
updateVals sc (x :: xs) = do
  v <- encodeExprP x.val
  updateVals (sc :< "\{x.name} = \{v}") xs

||| Encodes an SQLite data management command.
|||
||| The command will be encoded as a string with parameters
||| inserted as placeholders for literal values where appropriate.
|||
||| `State ParamST` is used to keep track of the defined parameters.
export
encodeCmd : Cmd t -> ParamStmt
encodeCmd (CREATE_TABLE t cs ifNotExists) =
  let CS m ts := foldl encConstraint (CS empty []) cs
      cols    := encodeCols m t.cols
      add     := commaSep id (cols ++ ts)
   in pure "CREATE TABLE \{ine ifNotExists} \{t.name} (\{add});"
encodeCmd (DROP_TABLE t ifExists) =
   pure "DROP TABLE \{ie ifExists} \{t.name};"
encodeCmd (INSERT t cs vs) = do
  vstr <- exprs [<] vs
  pure "INSERT INTO \{t.name} (\{insertCols [<] cs}) VALUES (\{vstr});"
encodeCmd (REPLACE t cs vs) = do
  vstr <- exprs [<] vs
  pure "REPLACE INTO \{t.name} (\{insertCols [<] cs}) VALUES (\{vstr});"
encodeCmd (UPDATE t vs wh) = do
  vstr <- updateVals [<] vs
  xstr <- encodeExprP wh
  pure "UPDATE \{t.name} SET \{vstr} WHERE \{xstr};"
encodeCmd (DELETE t wh) = do
  xstr <- encodeExprP wh
  pure "DELETE FROM \{t.name} WHERE \{xstr};"

||| Encodes an SQLite `SELECT` statement.
|||
||| The query will be encoded as a string with parameters
||| inserted as placeholders for literal values where appropriate.
export
encodeQuery : Query ts -> ParamStmt
encodeQuery (SELECT_FROM t vs where_) = do
  vstr <- exprs [<] vs
  wh   <- encodeExprP where_
  pure "SELECT \{vstr} FROM \{t.name} WHERE \{wh}"
