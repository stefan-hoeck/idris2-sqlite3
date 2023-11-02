module Sqlite3.Table

import Sqlite3.Types

%default total

||| A column in an SQLite table: A name paired with the type of
||| values stored in the column.
|||
||| Note: With this definition, we do include any constraints with
|||       our columns. These should be passed on separately when
|||       using the `CREATE_TABLE` command.
public export
record Column where
  constructor C
  name  : String
  type  : SqliteType

||| An SQLite table: A name paired with a list of columns.
public export
record Table where
  constructor T
  name : String
  cols : List Column

||| Inductive proof that a list of columns has a column
||| with the given string as its name.
public export
data ListHasCol : (cs : List Column) -> (s : String) -> Type where
  Here  : ListHasCol (C s t :: cs) s
  There : ListHasCol cs s -> ListHasCol (c::cs) s

||| Convenience alias for `ListHasCol` working on the columns
||| of a table.
public export
0 TableHasCol : Table -> String -> Type
TableHasCol = ListHasCol . cols

||| Computes the SQLite column type associated with column `n`
||| in the given list of columns.
public export
ListColType : (cs : List Column) -> ListHasCol cs n -> SqliteType
ListColType (C n t :: cs) Here      = t
ListColType (_     :: cs) (There x) = ListColType cs x

||| A column in the given table: This is just a column name
||| paired with a proof that the column exists in table `t`.
public export
record TColumn (t : Table) where
  constructor TC
  name : String
  {auto 0 prf : TableHasCol t name}

export %inline
fromString : (s : String) -> (0 p : TableHasCol t s) => TColumn t
fromString s = TC s

||| A database schema is a list of tables.
public export
0 Schema : Type
Schema = List Table

||| Proof that a column named `c` exists in a table named `n` in
||| schema `s`.
public export
data HasCol : (s : Schema) -> (n,c : String) -> Type where
  SHere  : ListHasCol t c => HasCol (T n t :: ts) n c
  SThere : HasCol ts n c -> HasCol (t::ts) n c

||| Computes the SQLite column type associated with column `c`
||| in table `n` in the given list of tables.
public export
ColType : (s : Schema) -> HasCol s n c -> SqliteType
ColType (T n cs ::_) (SHere @{p}) = ListColType cs p
ColType (_::xs)      (SThere p)   = ColType xs p
