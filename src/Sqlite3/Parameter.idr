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

encFun1 : String -> Expr s t -> ParamStmt

encFun : String -> List (Expr s t) -> ParamStmt

||| Encodes an expression, generating a list of parameters with
||| unique names that will be bound when running the SQL statement.
export
encodeExprP : Expr s t -> ParamStmt
encodeExprP (Lit t v)    =
  state $ \(PS x as) =>
    let s := ":\{show x}"
     in (PS (S x) (P s t v :: as), s)

encodeExprP (Add x y)    = encOp "+" x y
encodeExprP (Mult x y)   = encOp "*" x y
encodeExprP (Sub x y)    = encOp "-" x y
encodeExprP (Div x y)    = encOp "/" x y
encodeExprP (Mod x y)    = encOp "%" x y
encodeExprP (Abs y)       = encFun1 "abs" y
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
encodeExprP (Neg x)      = encPrefix "-" x
encodeExprP (NOT x)      = encPrefix "NOT" x
encodeExprP (Raw s)      = pure s
encodeExprP NULL         = pure "NULL"
encodeExprP TRUE         = pure "1"
encodeExprP FALSE        = pure "0"
encodeExprP (Col c)      = pure c
encodeExprP (COALESCE xs)     = encFun "coalesce" xs
encodeExprP (COUNT x)         = encFun1 "count" x
encodeExprP (AVG x)           = encFun1 "avg" x
encodeExprP (SUM x)           = encFun1 "sum" x
encodeExprP (MIN x)           = encFun1 "min" x
encodeExprP (MAX x)           = encFun1 "max" x
encodeExprP (GROUP_CONCAT x s) = do
  ex <- encodeExprP x
  pure "group_concat(\{ex}, \{s})"

encodeExprP CURRENT_TIME      = pure "CURRENT_TIME"
encodeExprP CURRENT_DATE      = pure "CURRENT_DATE"
encodeExprP CURRENT_TIMESTAMP = pure "CURRENT_TIMESTAMP"
encodeExprP (LIKE x y)        = encOp "LIKE" x y
encodeExprP (GLOB x y)        = encOp "GLOB" x y

encodeExprP (IN x xs) = do
  s  <- encodeExprP x
  ss <- encExprs [<] xs
  pure "(\{s}) IN (\{ss})"

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

encFun f xs      = do
  exs <- encExprs [<] xs
  pure "\{f}(\{exs})"

encFun1 f x      = do
  ex <- encodeExprP x
  pure "\{f}(\{ex})"

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

references : (t : SQLTable) -> LAll (TColumn t) xs -> String
references t cs = "REFERENCES \{t.name} (\{names [<] cs})"

action : ForeignKey.Action -> String
action SET_NULL    = "SET NULL"
action SET_DEFAULT = "SET DEFAULT"
action CASCADE     = "CASCADE"
action RESTRICT    = "RESTRICT"
action NO_ACTION   = "NO ACTION"

actions : ForeignKey.Action -> ForeignKey.Action -> String
actions NO_ACTION NO_ACTION = ""
actions update    NO_ACTION = " ON UPDATE \{action update}"
actions NO_ACTION delete    = " ON DELETE \{action delete}"
actions update    delete    = " ON UPDATE \{action update} ON DELETE \{action update}"

encDetails : Constraints -> ForeignKey.Details xs t -> Constraints
encDetails y (MkDetails s [p] ys u d) = addCol y p.name "\{references s ys}\{actions u d}" 
encDetails y (MkDetails s xs  ys u d) = addTbl y
  "FOREIGN KEY (\{names [<] xs}) \{references s ys} \{actions u d}"

encConstraint : Constraints -> Constraint t -> Constraints
encConstraint y (NOT_NULL $ TC n)      = addCol y n"NOT NULL"
encConstraint y (AUTOINCREMENT $ TC n) = addCol y n "AUTOINCREMENT"
encConstraint y (UNIQUE [TC n])        = addCol y n "UNIQUE"
encConstraint y (PRIMARY_KEY [TC n])   = addCol y n "PRIMARY KEY"
encConstraint y (DEFAULT s expr)       = addCol y s (encodeDflt expr)
encConstraint y (UNIQUE xs)            = addTbl y "UNIQUE (\{names [<] xs})"
encConstraint y (PRIMARY_KEY xs)       = addTbl y "PRIMARY KEY (\{names [<] xs})"
encConstraint y (CHECK x)              = addTbl y "CHECK (\{encodeExpr x})"
encConstraint y (ForeignKey details)   = encDetails y details

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

insertCols : SnocList String -> LAll (TColumn t) ts -> String
insertCols sc []         = commaSep id (sc <>> [])
insertCols sc (TC c::cs) = insertCols (sc :< c) cs

exprs : SnocList String -> LAll (Expr s) ts -> ParamStmt
exprs sc []      = pure $ commaSep id (sc <>> [])
exprs sc (c::cs) = do
  s <- encodeExprP c
  exprs (sc :< s) cs

namedExprs : SnocList String -> LAll (NamedExpr s) ts -> ParamStmt
namedExprs sc []      = pure $ commaSep id (sc <>> [])
namedExprs sc (AS c n::cs) = do
  s <- encodeExprP c
  let s2 := if n == "" then s else "\{s} AS \{n}"
  namedExprs (sc :< s2) cs

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
encodeCmd (CreateTable t cs ifNotExists) =
  let CS m ts := foldl encConstraint (CS empty []) cs
      cols    := encodeCols m t.cols
      add     := commaSep id (cols ++ ts)
   in pure "CREATE TABLE \{ine ifNotExists} \{t.name} (\{add});"
encodeCmd (DropTable t ifExists) =
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

joinPred : JoinPred s t -> ParamStmt
joinPred (Left u)  = pure "USING (\{commaSep name u})"
joinPred (Right x) = do
  ez <- encodeExprP x
  pure "ON \{ez}"

tbl : SQLTable -> String
tbl (ST n a _) = if n == a then n else "\{n} AS \{a}"

join : Join s t -> ParamStmt
join (JOIN t p) = do
  ep <- joinPred p
  pure "JOIN \{tbl t} \{ep}"

join (OUTER_JOIN t p) = do
  ep <- joinPred p
  pure "LEFT OUTER JOIN \{tbl t} \{ep}"

join (CROSS_JOIN t) = pure "CROSS JOIN \{tbl t}"
join (FROM t)       = pure "FROM \{tbl t}"

encodeFrom : From s -> ParamStmt
encodeFrom [<]      = pure ""
encodeFrom [<x]     = join x
encodeFrom (x :< y) = do
  ef <- encodeFrom x
  ej <- join y
  pure "\{ef} \{ej}"

asc : AscDesc -> String
asc NoAsc = ""
asc Asc   = "Asc"
asc Desc  = "Desc"

collate : Collation t -> String
collate None   = ""
collate NOCASE = "COLLATE NOCASE"

encodeOrderingTerm : OrderingTerm t -> ParamStmt
encodeOrderingTerm (O expr coll a) = do
  ex <- encodeExprP expr
  pure "\{ex} \{collate coll} \{asc a}"

ots : SnocList String -> List (OrderingTerm t) -> ParamStmt
ots ss []      = pure $ commaSep id (ss <>> [])
ots ss (x::xs) = do
  s <- encodeOrderingTerm x
  ots (ss :< s) xs

encodeOrd : String -> List (OrderingTerm t) -> ParamStmt
encodeOrd s [] = pure ""
encodeOrd s xs = do
  str <- ots [<] xs
  pure "\{s} \{str}"

limit : Maybe Nat -> Nat -> String
limit Nothing  0 = ""
limit (Just n) 0 = "LIMIT \{show n}"
limit x n = let lim := maybe "-1" show x in "LIMIT \{lim} OFFSET \{show n}"

encodeHaving : Expr s t -> ParamStmt
encodeHaving TRUE = pure ""
encodeHaving x    = do
  s <- encodeExprP x
  pure "HAVING \{s}"

||| Encodes an SQLite `SELECT` statement.
|||
||| The query will be encoded as a string with parameters
||| inserted as placeholders for literal values where appropriate.
export
encodeQuery : Query ts -> ParamStmt
encodeQuery (Q distinct _ from vs where_ having group_by order_by lim off) = do
  vstr <- namedExprs [<] vs
  fstr <- encodeFrom from
  wh   <- encodeExprP where_
  hav  <- encodeHaving having
  grp  <- encodeOrd "GROUP BY" (map ord group_by)
  ord  <- encodeOrd "ORDER BY" order_by
  let rest = "\{vstr} \{fstr} WHERE \{wh} \{grp} \{hav} \{ord} \{limit lim off}"
  pure $ case distinct of
    True  => "SELECT DISTINCT \{rest}"
    False => "SELECT \{rest}"
