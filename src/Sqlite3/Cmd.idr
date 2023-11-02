module Sqlite3.Cmd

import Sqlite3.Expr
import Sqlite3.Table
import Sqlite3.Types

%default total

namespace Columns
  ||| A list of columns in a table indexed by the corresponding
  ||| column type.
  |||
  ||| This is used for inserting values into a table using the `INSERT`
  ||| command: Column names come in one list, while a corresponding
  ||| list of values (expressions) comes in a second list (see
  ||| `Sqlite3.Cmd.Values.Values`).
  |||
  ||| Note: Typically, the `ts` index of a column list is derived
  |||       from a columns name and the proof of the name being in
  |||       a table's list of columns.
  public export
  data Columns : (t : Table) -> (ts : List SqliteType) -> Type where
    Nil  : Columns t []
    (::) :
         {0 t        : Table}
      -> {0 ts       : List SqliteType}
      -> (s          : String)
      -> {auto 0 prf : TableHasCol t s}
      -> (cs         : Columns t ts)
      -> Columns t (ListColType t.cols prf :: ts)

  ||| Concatenates two lists of columns.
  export
  (++) : Columns t xs -> Columns t ys -> Columns t (xs ++ ys)
  (++) []     ws = ws
  (++) (h::t) ws = h :: (t ++ ws)

namespace Values
  ||| A list of expressions to be inserted, updated, or selected
  ||| in a table.
  public export
  data Values : (t : Table) -> (ts : List SqliteType) -> Type where
    Nil  : Values t []
    (::) : Expr [t] x -> Values t ts -> Values t (x::ts)

  ||| Concatenates two lists of expressions.
  export
  (++) : Values t xs -> Values t ys -> Values t (xs ++ ys)
  (++) []     ws = ws
  (++) (h::t) ws = h :: (t ++ ws)

||| A single name-value pair in an `UPDATE` statement.
public export
record Val (t : Table) where
  constructor V
  name        : String
  {auto 0 prf : TableHasCol t name}
  val         : Expr [t] (ListColType t.cols prf)

||| Column and table constraints to be used when creating a new table.
public export
data Constraint : Table -> Type where
  NotNull       : TColumn t -> Constraint t
  AutoIncrement : TColumn t -> Constraint t
  Unique        : List (TColumn t) -> Constraint t
  PrimaryKey    : List (TColumn t) -> Constraint t
  ForeignKey    : List (TColumn t) -> Constraint t
  Check         : Expr [t] INTEGER -> Constraint t
  Default       :
       {0 t        : Table}
    -> (s          : String)
    -> {auto 0 prf : TableHasCol t s}
    -> (expr       : Expr [t] (ListColType t.cols prf))
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
    -> (cols : Columns t ts)
    -> (vals : Values t ts)
    -> Cmd TInsert

  REPLACE :
       {0 ts : List SqliteType}
    -> (t : Table)
    -> (cols : Columns t ts)
    -> (vals : Values t ts)
    -> Cmd TInsert

  UPDATE :
       (t      : Table)
    -> (set    : List (Val t))
    -> (where_ : Expr [t] INTEGER)
    -> Cmd TUpdate

  DELETE :
       (t      : Table)
    -> (where_ : Expr [t] INTEGER)
    -> Cmd TDelete

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

||| Different types of `SELECT` commands.
public export
data Query : (ts : List SqliteType) -> Type where
  SELECT_FROM :
       {0 ts   : List SqliteType}
    -> (t      : Table)
    -> (vs     : Values t ts)
    -> (where_ : Expr [t] INTEGER)
    -> Query ts
