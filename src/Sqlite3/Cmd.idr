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
  val         : Expr [t] (TableColType name t)

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

  Check         : Expr [t] BOOL -> Constraint t
  Default       :
       {0 t        : Table}
    -> (s          : String)
    -> {auto 0 prf : IsJust (FindCol s t.cols)}
    -> (expr       : Expr [t] (TableColType s t))
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
    -> (vals : LAll (Expr [t]) ts)
    -> Cmd TInsert

  REPLACE :
       {0 ts : List SqliteType}
    -> (t : Table)
    -> (cols : LAll (TColumn t) ts)
    -> (vals : LAll (Expr [t]) ts)
    -> Cmd TInsert

  UPDATE :
       (t      : Table)
    -> (set    : List (Val t))
    -> (where_ : Expr [t] BOOL)
    -> Cmd TUpdate

  DELETE :
       (t      : Table)
    -> (where_ : Expr [t] BOOL)
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

||| The `FROM` part in a select statement.
public export
data From : (s : Schema) -> Type where
  Tbl     : (t : Table) -> From [t]
  TblAs   : (t : Table) -> (name : String) -> From [T name t.cols]
  Cross   : From s -> From t -> From (s ++ t)
  Natural : From s -> From t -> From (s ++ t)

  JoinOn  :
       From s
    -> From t
    -> (outer : Bool)
    -> (on    : Expr (s ++ t) BOOL)
    -> From (s++t)

  JoinUsing  :
       From s
    -> From t
    -> (outer  : Bool)
    -> (using_ : List (JColumn s t))
    -> From (s++t)

||| Different types of `SELECT` commands.
public export
data Query : Type -> Type where
  SELECT :
       {auto as : AsRow t}
    -> (from    : From s)
    -> (xs      : LAll (Expr s) (RowTypes t))
    -> (where_  : Expr s BOOL)
    -> Query t
