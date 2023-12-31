module Sqlite3.Table

import public Data.List1
import public Data.Maybe
import public Data.SnocList
import public Data.String
import Data.Bits
import Data.Buffer.Indexed
import Data.ByteString
import Sqlite3.Marshall
import Sqlite3.Types

%default total

--------------------------------------------------------------------------------
-- SQL Columns and Tables
--------------------------------------------------------------------------------

||| A column in an SQLite table: A name paired with the type of
||| values stored in the column.
|||
||| Note: With this definition, we do not include any constraints with
|||       our columns. These should be passed on separately when
|||       using the `CREATE_TABLE` command.
public export
record Column where
  constructor C
  name  : String
  type  : SqliteType

||| An SQLite table: A name paired with a list of columns.
public export
record SQLTable where
  constructor ST
  ||| The table's name
  name : String

  ||| Field `as` is used to rename tables within a `SELECT` statement.
  ||| This is, for instance, needed when `JOIN`ing a table with itself.
  as   : String

  ||| The list of table columns
  cols : List Column

||| Utility constructor for tables that sets fields `name` and `as`
||| both the value of the string argument.
public export %inline
table : String -> List Column -> SQLTable
table n = ST n n

||| Utility to change the name of a table in a `SELECT` statement.
|||
||| The name was chosen to resemble SQL syntax, and it is recommended
||| to use this in infix notation:
|||
||| ```idris
||| students `AS` "st"
||| ```
public export %inline
AS : SQLTable -> String -> SQLTable
AS (ST n _ cs) as = ST n as cs

||| Computes the types of columns stored in a table.
public export %inline
ColTypes : SQLTable -> List SqliteType
ColTypes = map type . cols

||| Tries to look up a column type by name
||| in a list of columns.
|||
||| We use this often in proofs, therefore this comes with
||| an upper-case name.
public export
FindCol : String -> List Column -> Maybe SqliteType
FindCol s []        = Nothing
FindCol s (x :: xs) = if x.name == s then Just x.type else FindCol s xs

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
  -> (t        : SQLTable)
  -> {auto 0 p : IsJust (FindCol s t.cols)}
  -> SqliteType
TableColType s t = fromJust (FindCol s t.cols)

--------------------------------------------------------------------------------
-- Typed SQL Columns
--------------------------------------------------------------------------------

||| A column in the given table: This is just a column name
||| paired with a proof that the column exists in table `t`
||| and has type `tpe`.
public export
data TColumn : (t : SQLTable) -> (tpe : SqliteType) -> Type where
  TC :
       {0 t        : SQLTable}
    -> (name       : String)
    -> {auto 0 prf : IsJust (FindCol name t.cols)}
    -> TColumn t (TableColType name t)

||| Extracts the name of a `TColumn`.
public export %inline
(.name) : TColumn t x -> String
(.name) (TC n) = n

namespace TColumn
  public export %inline
  fromString :
       {0 t      : SQLTable}
    -> (name     : String)
    -> {auto 0 p : IsJust (FindCol name t.cols)}
    -> TColumn t (TableColType name t)
  fromString = TC

--------------------------------------------------------------------------------
-- Schemata
--------------------------------------------------------------------------------

||| A database schema is a (snoc-)list of tables.
|||
||| The reason for preferring `SnocList` over `List` is that we
||| typically build up a schema via joins in a query, and it is
||| more natural to append table from left to right in a complex
||| join statement than the other way round.
public export
0 Schema : Type
Schema = SnocList SQLTable

||| Looks up a table and column name in a schema.
|||
||| This is called from `FindSchemaCol` after verifying that the
||| column identifier has been fully qualified by prefixing it with
||| a table name.
|||
||| The table name is checked against the `as` field of the tables.
||| This allows us to rename tables as part of a `SELECT` statement,
||| which is especially important when joining a table with itself.
public export
FindSchemaCol2 : (table, column : String) -> Schema -> Maybe SqliteType
FindSchemaCol2 t c [<]       = Nothing
FindSchemaCol2 t c (sx :< x) =
  if x.as == t then FindCol c x.cols else FindSchemaCol2 t c sx

||| Looks up an unqualified column name in a schema.
|||
||| In order for this to succeed, the schema must either consist of
||| only a single table, or the last table in the schema must be unnamed.
|||
||| The latter case (the last table being unnamed) occurs, when we
||| give column names to arbitrary SQL expressions in a `SELECT`
||| statement. Since these expressions are evaluated after the
||| rest of the schema is known (the rest of the schema comes from
||| the `FROM` part of a `SELECT` statement), it is natural to just
||| append an unnamed table with custom column names at the end of
||| the schema.
public export
FindSchemaCol1 : String -> Schema -> Maybe SqliteType
FindSchemaCol1 n [< t]             = FindCol n t.cols
FindSchemaCol1 n (_:< ST "" "" cs) = FindCol n cs
FindSchemaCol1 n _                 = Nothing

||| Looks up a - possibly qualified - column name in a schema.
|||
||| This invokes `FindSchemaCol1` or `FindSchemaCol2` depending on
||| whether the column name is fully qualified or not. A qualified
||| column name is prefixed with the corresponding table's name
||| followed by a dot: `"employees.name"`.
public export
FindSchemaCol : String -> Schema -> Maybe SqliteType
FindSchemaCol str s =
  case split ('.' ==) str of
    n:::[]  => FindSchemaCol1 n s
    t:::[n] => FindSchemaCol2 t n s
    _       => Nothing

||| Computes the SQLite column type associated with column `column`
||| in table `table` in the given list of tables.
public export
SchemaColType :
     (name : String)
  -> (s             : Schema)
  -> {auto 0 prf    : IsJust (FindSchemaCol name s)}
  -> SqliteType
SchemaColType n s = fromJust (FindSchemaCol n s)

||| Proof that a column with the given name exists in
||| the given list of tables.
public export
SchemaHasCol : Schema -> String -> Bool
SchemaHasCol [<]       s = False
SchemaHasCol (sx :< x) s = any ((s==) . name) x.cols || SchemaHasCol sx s

--------------------------------------------------------------------------------
-- Shared Columns
--------------------------------------------------------------------------------

||| A column used in a `JOIN ... USING` statement: The column name must
||| appear in both schemata.
public export
record JColumn (s : Schema) (t : SQLTable) where
  constructor JC
  name       : String
  {auto 0 p1 : SchemaHasCol s name === True}
  {auto 0 p2 : IsJust (FindCol name t.cols)}

namespace JColumn
  public export %inline
  fromString :
       {0 s       : Schema}
    -> {0 t       : SQLTable}
    -> (name      : String)
    -> {auto 0 p1 : SchemaHasCol s name === True}
    -> {auto 0 p2 : IsJust (FindCol name t.cols)}
    -> JColumn s t
  fromString = JC

--------------------------------------------------------------------------------
-- Idris Tables
--------------------------------------------------------------------------------

||| A table of values together with a fitting header
public export
record Table a where
  constructor T
  {auto torow : ToRow a}
  header : LAll (const String) (ToRowTypes a)
  rows   : List a

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

export %inline
quote : Char
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

encode : (t : SqliteType) -> Maybe (IdrisType t) -> String
encode _ Nothing  = "NULL"
encode BLOB    (Just v) = encodeBytes v
encode TEXT    (Just v) = v
encode INTEGER (Just v) = show v
encode REAL    (Just v) = show v

pad : SqliteType -> Nat -> String -> String
pad BLOB    k = padRight k ' '
pad TEXT    k = padRight k ' '
pad INTEGER k = padLeft k ' '
pad REAL    k = padLeft k ' '

center : Nat -> String -> String
center k s =
  let ki := cast {to = Integer} (k `minus` length s)
      pl := ki `div` 2
      pr := ki - pl
   in replicate (cast pl) ' ' ++ s ++ replicate (cast pr) ' '

maxLength : Nat -> String -> Nat
maxLength n = max n . length

barSep : LAll (const String) ts -> String
barSep = fastConcat . intersperse " | " . forget

bar : LAll (const Nat) ts -> String
bar = fastConcat . intersperse "---" . map (`replicate` '-') . forget

export
prettyRows :
     {ts : _}
  -> (header : LAll (const String) ts)
  -> (rows   : List (LAll (Maybe . IdrisType) ts))
  -> String
prettyRows h rs =
  let cells   := map (hmapW encode) rs
      lengths := foldl (hzipWith maxLength) (hconst 0) (h :: cells)
      rows    := map (barSep . hzipWithW pad lengths) cells
      header  := barSep $ hzipWith center lengths h
   in fastUnlines (header :: bar lengths :: rows)

export
prettyTable : Table a -> String
prettyTable (T h rs) = prettyRows h (map toRow rs)

export %inline
printTable : HasIO io => Table a -> io ()
printTable = putStrLn . prettyTable
