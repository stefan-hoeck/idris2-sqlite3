module Sqlite3.Cmd

import Data.List.Quantifiers
import Sqlite3.Expr
import Sqlite3.Marshall
import Sqlite3.Table
import Sqlite3.Types

%default total

||| We can always convert a list of marshallable Idris values to
||| a list of SQL expressions.
export
toExprs : {ts : _} -> LAll (Maybe . IdrisType) ts -> LAll (Expr s) ts
toExprs {ts = []}    []      = []
toExprs {ts = t::ts} (v::vs) = maybe NULL (Lit t) v :: toExprs vs

||| A single name-value pair in an `UPDATE` statement.
public export
record Val (t : Table) where
  constructor V
  name        : String
  {auto 0 prf : IsJust (FindCol name t.cols)}
  val         : Expr [<t] (TableColType name t)

||| Column and table constraints to be used when creating a new table.
public export
data Constraint : Table -> Type where
  NotNull       : {0 x : _} -> TColumn t x -> Constraint t
  AutoIncrement : {0 x : _} -> TColumn t x -> Constraint t
  Unique        : {0 xs : _} -> LAll (TColumn t) xs -> Constraint t
  PrimaryKey    : {0 xs : _} -> LAll (TColumn t) xs -> Constraint t
  ForeignKey    :
       {0 xs : _}
    -> (s       : Table)
    -> LAll (TColumn t) xs
    -> LAll (TColumn s) xs
    -> Constraint t

  Check         : Expr [<t] BOOL -> Constraint t
  Default       :
       {0 t        : Table}
    -> (s          : String)
    -> {auto 0 prf : IsJust (FindCol s t.cols)}
    -> (expr       : Expr [<t] (TableColType s t))
    -> Constraint t

||| Index used to distinguish different types of commands.
|||
||| This should facilitate writing combinator such as
||| `if_not_exists` to operate only on certain commands.
public export
data CmdType : Type where
  TCreate : CmdType
  TDelete : CmdType
  TDrop   : CmdType
  TInsert : CmdType
  TSelect : CmdType
  TUpdate : CmdType

||| Data management commands for creating tables and
||| inserting, updating, or deleting rows in a table.
public export
data Cmd : CmdType -> Type where
  CREATE_TABLE :
       (t           : Table)
    -> (attrs       : List (Constraint t))
    -> (ifNotExists : Bool)
    -> Cmd TCreate

  DROP_TABLE : (t : Table) -> (ifExists : Bool) -> Cmd TDrop

  INSERT :
       {0 ts : List SqliteType}
    -> (t : Table)
    -> (cols : LAll (TColumn t) ts)
    -> (vals : LAll (Expr [<t]) ts)
    -> Cmd TInsert

  REPLACE :
       {0 ts : List SqliteType}
    -> (t : Table)
    -> (cols : LAll (TColumn t) ts)
    -> (vals : LAll (Expr [<t]) ts)
    -> Cmd TInsert

  UPDATE :
       (t      : Table)
    -> (set    : List (Val t))
    -> (where_ : Expr [<t] BOOL)
    -> Cmd TUpdate

  DELETE :
       (t      : Table)
    -> (where_ : Expr [<t] BOOL)
    -> Cmd TDelete

export
insert :
    {auto as : AsRow v}
  ->(t      : Table)
  -> LAll (TColumn t) (RowTypes v)
  -> (value : v)
  -> Cmd TInsert
insert t cs value = INSERT t cs (toExprs $ toRow value)

namespace Cmds
  ||| A list of different types of commands, with the
  ||| command types being hidden.
  public export
  data Cmds : Type where
    Nil  : Cmds
    (::) : {0 t : _} -> Cmd t -> Cmds -> Cmds

  export
  (++) : Cmds -> Cmds -> Cmds
  []     ++ cs = cs
  (h::t) ++ cs = h :: (t ++ cs)

  export %inline
  Semigroup Cmds where (<+>) = (++)

  export %inline
  Monoid Cmds where neutral = []

  export
  fromList : List (Cmd t) -> Cmds
  fromList []      = []
  fromList (c::cs) = c :: fromList cs

||| Create a table only if it does not yet exist.
public export
if_not_exists : Cmd TCreate -> Cmd TCreate
if_not_exists (CREATE_TABLE t attrs _) = CREATE_TABLE t attrs True

||| Drop a table only if it exists.
public export
if_exists : Cmd TDrop -> Cmd TDrop
if_exists (DROP_TABLE t _) = DROP_TABLE t True

||| Convenience constructor for the `CREATE_TABLE` command
public export %inline
createTable : (t : Table) -> List (Constraint t) -> Cmd TCreate
createTable t as = CREATE_TABLE t as False

||| Convenience constructor for the `DROP_TABLE` command
public export %inline
dropTable : (t : Table) -> Cmd TDrop
dropTable t = DROP_TABLE t False

public export
0 JoinPred : Schema -> Table -> Type
JoinPred s t = Either (List $ JColumn  s t) (Expr (s:<t) BOOL)

public export
data Join : (s : Schema) -> (t : Table) -> Type where
  JOIN :
       {0 t0 : Table}
    -> {0 s  : Schema}
    -> (t : Table)
    -> JoinPred (s:<t0) t
    -> Join (s:<t0) t

  OUTER_JOIN :
       {0 t0 : Table}
    -> {0 s  : Schema}
    -> (t : Table)
    -> JoinPred (s:<t0) t
    -> Join (s:<t0) t

  CROSS_JOIN : {0 t0 : _} -> {0 s : _} -> (t : Table) -> Join (s:<t0) t

  FROM : (t : Table) -> Join [<] t

public export %inline
USING : (JoinPred s t -> Join s t) -> List (JColumn s t) -> Join s t
USING f = f . Left

public export %inline
ON : (JoinPred s t -> Join s t) -> Expr (s:<t) BOOL -> Join s t
ON f = f . Right

||| The `FROM` part in a select statement.
public export
data From : (s : Schema) -> Type where
  Lin  : From [<]
  (:<) : From s -> Join s t -> From (s :< t)

||| Tag indicating, whether results should be sorted in ascending
||| or descending order.
public export
data AscDesc = NoAsc | ASC | DESC

||| Different collations used during ordering.
|||
||| Currently, only `NO CASE` is supported.
public export
data Collation : (t : SqliteType) -> Type where
  None   : Collation t
  NOCASE : Collation TEXT

public export
record OrderingTerm (s : Schema) where
  constructor O
  {0 tpe : SqliteType}
  expr : Expr s tpe
  coll : Collation tpe
  asc  : AscDesc

||| Different types of `SELECT` commands.
public export
data Query : Type -> Type where
  SELECT :
       {auto as   : AsRow t}
    -> (xs        : LAll (Expr s) (RowTypes t))
    -> (from      : From s)
    -> (where_    : Expr s BOOL)
    -> (group_by  : List (OrderingTerm s))
    -> (order_by  : List (OrderingTerm s))
    -> Query t

public export
0 LQuery : List Type -> Type
LQuery = Query . HList
