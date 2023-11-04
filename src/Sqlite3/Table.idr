module Sqlite3.Table

import public Data.Maybe
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

public export %inline
ColTypes : Table -> List SqliteType
ColTypes = map type . cols

||| Tries and looks up a column type by name
||| in a list of columns.
|||
||| We use this often in proofs, therefore this comes with
||| an upper-case name.
public export
FindCol : String -> List Column -> Maybe SqliteType
FindCol s []        = Nothing
FindCol s (x :: xs) =
  if x.name == s then Just x.type else FindCol s xs

||| Column type of a column (given by name) in a list of
||| columns.
|||
||| This requires a proof that the column actually is in the
||| table.
public export %inline
ListColType :
     (s        : String)
  -> (cs       : List Column)
  -> {auto 0 p : IsJust (FindCol s cs)}
  -> SqliteType
ListColType s cs = fromJust (FindCol s cs)

||| Column type of a column (given by name) in a table.
|||
||| This requires a proof that the column actually is in the
||| table.
public export %inline
TableColType :
     (s        : String)
  -> (t        : Table)
  -> {auto 0 p : IsJust (FindCol s t.cols)}
  -> SqliteType
TableColType s t = fromJust (FindCol s t.cols)

||| A column in the given table: This is just a column name
||| paired with a proof that the column exists in table `t`.
public export
data TColumn : (t : Table) -> (tpe : SqliteType) -> Type where
  TC :
       {0 t        : Table}
    -> (name       : String)
    -> {auto 0 prf : IsJust (FindCol name t.cols)}
    -> TColumn t (TableColType name t)

public export %inline
fromString :
     {0 t      : Table}
  -> (name     : String)
  -> {auto 0 p : IsJust (FindCol name t.cols)}
  -> TColumn t (TableColType name t)
fromString = TC

||| A database schema is a list of tables.
public export
0 Schema : Type
Schema = List Table

||| Looks up a table and column name in a schema.
public export
FindSchemaCol :
     (table, column : String)
  -> Schema
  -> Maybe SqliteType
FindSchemaCol t c []        = Nothing
FindSchemaCol t c (x :: xs) =
  if x.name == t then FindCol c x.cols else FindSchemaCol t c xs

||| Computes the SQLite column type associated with column `column`
||| in table `table` in the given list of tables.
public export
SchemaColType :
     (table, column : String)
  -> (s             : Schema)
  -> {auto 0 prf    : IsJust (FindSchemaCol table column s)}
  -> SqliteType
SchemaColType t c s = fromJust (FindSchemaCol t c s)
