module Sqlite3.Parameter

import public Control.Monad.State

import Data.Buffer.Indexed
import Data.ByteString
import Data.Maybe
import Data.SortedMap
import Data.String

import Sqlite3.Cmd
import Sqlite3.Expr
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

||| Encodes an expression, generating a list of parameters with
||| unique names that will be bound when running the SQL statement.
export
encodeExprP : Expr s t -> ParamStmt
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
encodeExprP (Lit t v)    =
  state $ \(PS x as) =>
    let s := ":\{show x}"
     in (PS (S x) (P s t v :: as), s)

encOp s x y = do
  sx <- encodeExprP x
  sy <- encodeExprP y
  pure $ "(\{sx} \{s} \{sy})"

encPrefix s x = do
  sx <- encodeExprP x
  pure $ "\{s}(\{sx})"

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

commaSep : (a -> String) -> List a -> String
commaSep f = concat . intersperse ", " . map f

names : List (TColumn t) -> String
names = commaSep name

encodeDflt : Expr s t -> String
encodeDflt x         = "DEFAULT (\{encodeExpr x})"

encConstraint : Constraints -> Constraint t -> Constraints
encConstraint y (NotNull c)       = addCol y c.name "NOT NULL"
encConstraint y (AutoIncrement c) = addCol y c.name "AUTOINCREMENT"
encConstraint y (Unique [c])      = addCol y c.name "UNIQUE"
encConstraint y (PrimaryKey [c])  = addCol y c.name "PRIMARY KEY"
encConstraint y (ForeignKey [c])  = addCol y c.name "FOREIGN KEY"
encConstraint y (Default s expr)  = addCol y s (encodeDflt expr)
encConstraint y (Unique xs)       = addTbl y "UNIQUE (\{names xs})"
encConstraint y (PrimaryKey xs)   = addTbl y "PRIMARY KEY (\{names xs})"
encConstraint y (ForeignKey xs)   = addTbl y "FOREIGN KEY (\{names xs})"
encConstraint y (Check x)         = addTbl y "CHECK (\{encodeExpr x})"

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
insertCols sc []      = commaSep id (sc <>> [])
insertCols sc (c::cs) = insertCols (sc :< c) cs

values : SnocList String -> Values t ts -> ParamStmt
values sc []      = pure $ commaSep id (sc <>> [])
values sc (c::cs) = do
  s <- encodeExprP c
  values (sc :< s) cs

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
  vstr <- values [<] vs
  pure "INSERT INTO \{t.name} (\{insertCols [<] cs}) VALUES (\{vstr});"
encodeCmd (REPLACE t cs vs) = do
  vstr <- values [<] vs
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
  vstr <- values [<] vs
  wh   <- encodeExprP where_
  pure "SELECT \{vstr} FROM \{t.name} WHERE \{wh}"

--------------------------------------------------------------------------------
-- Runnings Commands
--------------------------------------------------------------------------------
--
-- export %inline
-- cmd : Has SqlError es => DB => Cmd t -> App es ()
-- cmd = commitParam . encodeCmd
--
-- export %inline
-- query : {ts : _} -> Has SqlError es => DB => Query ts -> App es (Schema.Table ts)
-- query q =
--   let (PS _ args, str) := runState init (encodeQuery q)
--    in withBindStmt str args (injectIO $ loadRows 10000000)
--
-- rollback : Has SqlError es => DB => HSum es -> App es a
-- rollback x = ignore (withStmt "ROLLBACK TRANSACTION" step) >> fail x
--
-- ||| Runs several commands in a single transaction.
-- |||
-- ||| If any of the commands fails, the whole transaction is rolled back.
-- export %inline
-- cmds : Has SqlError es => DB => Cmds -> App es ()
-- cmds cs = do
--   ignore $ withStmt "BEGIN TRANSACTION" step
--   catch rollback (runCommands cs)
--   ignore $ withStmt "COMMIT TRANSACTION" step
--
--   where
--     runCommands : Cmds -> App es ()
--     runCommands []      = pure ()
--     runCommands (c::cs) = cmd c >> runCommands cs
--
-- --------------------------------------------------------------------------------
-- -- Examples
-- --------------------------------------------------------------------------------
--
-- Users : Table
-- Users =
--   T
--     "users"
--     [ C "id"    INTEGER
--     , C "name"  TEXT
--     , C "email" TEXT
--     , C "age"   INTEGER
--     ]
--
-- Settings : Table
-- Settings =
--   T
--     "settings"
--     [ C "id"    INTEGER
--     , C "json"  TEXT
--     ]
--
-- Files : Table
-- Files =
--   T
--     "files"
--     [ C "id"       INTEGER
--     , C "hash"     BLOB
--     , C "content"  BLOB
--     ]
--
-- createUsers : Cmd TCreate
-- createUsers =
--   if_not_exists $ createTable Users
--     [ PrimaryKey ["id"]
--     , AutoIncrement "id"
--     , NotNull "name"
--     , Unique ["name"]
--     , NotNull "age"
--     , Default "age" 18
--     , Check $ C "age" >= 2
--     , NotNull "email"
--     , Default "email" "foo'bar"
--     ]
--
-- insertUser : Values Users [TEXT,TEXT,INTEGER] -> Cmd TInsert
-- insertUser = INSERT Users ["name", "email", "age"]
--
-- replaceUser : Values Users [TEXT,TEXT,INTEGER] -> Cmd TInsert
-- replaceUser = REPLACE Users ["name", "email", "age"]
--
-- 0 Errs : List Type
-- Errs = [SqlError]
--
-- handlers : All (Handler ()) Errs
-- handlers = [printLn]
--
-- q : Query [INTEGER,TEXT,TEXT]
-- q = SELECT_FROM Users [C "id", C "name", C "email"] 1
--
-- main : IO ()
-- main = runApp handlers $ withDB "/data/cyby/cyby.sqlite" $ do
--   vs <- query (SELECT_FROM Files [C "id", C "content"] 1)
--   traverse_ (\[i,j] => printLn $  maybe 0 length j) vs

-- main = runApp handlers $ withDB ":memory"" $ do
--   cmds
--     [ createUsers
--     , insertUser ["Gundi", "hock@zhaw.ch", 44]
--     , insertUser ["Ruehli", "esthi@ewz.ch", 39]
--     , insertUser ["Baby", NULL, 0]
--     , insertUser ["Yannis", "", 8]
--     , insertUser ["Ronja", "ronja@gmail.com", 10]
--     , insertUser ["Niklas", "niklas@gmail.com", 12]
--     , replaceUser ["Gundi", "efa@gemail.ch", 50]
--     ]
--   vs <- query q
--   traverse_ printLn vs
--   cmd (UPDATE Users [V "email" "hock@zhaw.ch"] (C "age" > 40))
--   ws <- query q
--   traverse_ printLn ws
--   cmd (DELETE Users (C "name" == "Gundi"))
--   cmd (dropTable Users)
