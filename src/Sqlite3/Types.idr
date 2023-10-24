module Sqlite3.Types

import Data.Buffer.Indexed
import Data.List.Quantifiers
import Derive.Prelude

%default total
%language ElabReflection

--------------------------------------------------------------------------------
--          Status Info
--------------------------------------------------------------------------------

||| Possible result codes when interacting with the SQLite C interface.
|||
||| These result codes are described in greater detail in the
||| [SQLite C interface documentation](https://www.sqlite.org/rescode.html).
public export
data SqlResult : Type where
  SQLITE_OK         : SqlResult -- Successful result
  SQLITE_ERROR      : SqlResult -- Generic error
  SQLITE_INTERNAL   : SqlResult -- Internal logic error in SQLite
  SQLITE_PERM       : SqlResult -- Access permission denied
  SQLITE_ABORT      : SqlResult -- Callback routine requested an abort
  SQLITE_BUSY       : SqlResult -- The database file is locked
  SQLITE_LOCKED     : SqlResult -- A table in the database is locked
  SQLITE_NOMEM      : SqlResult -- A malloc() failed
  SQLITE_READONLY   : SqlResult -- Attempt to write a readonly database
  SQLITE_INTERRUPT  : SqlResult -- Operation terminated by sqlite3_interrupt()
  SQLITE_IOERR      : SqlResult -- Some kind of disk I/O error occurred
  SQLITE_CORRUPT    : SqlResult -- The database disk image is malformed
  SQLITE_NOTFOUND   : SqlResult -- Unknown opcode in sqlite3_file_control()
  SQLITE_FULL       : SqlResult -- Insertion failed because database is full
  SQLITE_CANTOPEN   : SqlResult -- Unable to open the database file
  SQLITE_PROTOCOL   : SqlResult -- Database lock protocol error
  SQLITE_EMPTY      : SqlResult -- Internal use only
  SQLITE_SCHEMA     : SqlResult -- The database schema changed
  SQLITE_TOOBIG     : SqlResult -- String or BLOB exceeds size limit
  SQLITE_CONSTRAINT : SqlResult -- Abort due to constraint violation
  SQLITE_MISMATCH   : SqlResult -- Data type mismatch
  SQLITE_MISUSE     : SqlResult -- Library used incorrectly
  SQLITE_NOLFS      : SqlResult -- Uses OS features not supported on host
  SQLITE_AUTH       : SqlResult -- Authorization denied
  SQLITE_FORMAT     : SqlResult -- Not used
  SQLITE_RANGE      : SqlResult -- 2nd parameter to sqlite3_bind out of range
  SQLITE_NOTADB     : SqlResult -- File opened that is not a database file
  SQLITE_NOTICE     : SqlResult -- Notifications from sqlite3_log()
  SQLITE_WARNING    : SqlResult -- Warnings from sqlite3_log()
  SQLITE_ROW        : SqlResult -- sqlite3_step() has another row ready
  SQLITE_DONE       : SqlResult -- sqlite3_step() has finished executing
  Unknown           : SqlResult -- error code unknown

%runElab derive "SqlResult" [Show,Eq,Ord]

public export
fromInt : Int -> SqlResult
fromInt 0   = SQLITE_OK
fromInt 1   = SQLITE_ERROR
fromInt 2   = SQLITE_INTERNAL
fromInt 3   = SQLITE_PERM
fromInt 4   = SQLITE_ABORT
fromInt 5   = SQLITE_BUSY
fromInt 6   = SQLITE_LOCKED
fromInt 7   = SQLITE_NOMEM
fromInt 8   = SQLITE_READONLY
fromInt 9   = SQLITE_INTERRUPT
fromInt 10  = SQLITE_IOERR
fromInt 11  = SQLITE_CORRUPT
fromInt 12  = SQLITE_NOTFOUND
fromInt 13  = SQLITE_FULL
fromInt 14  = SQLITE_CANTOPEN
fromInt 15  = SQLITE_PROTOCOL
fromInt 16  = SQLITE_EMPTY
fromInt 17  = SQLITE_SCHEMA
fromInt 18  = SQLITE_TOOBIG
fromInt 19  = SQLITE_CONSTRAINT
fromInt 20  = SQLITE_MISMATCH
fromInt 21  = SQLITE_MISUSE
fromInt 22  = SQLITE_NOLFS
fromInt 23  = SQLITE_AUTH
fromInt 24  = SQLITE_FORMAT
fromInt 25  = SQLITE_RANGE
fromInt 26  = SQLITE_NOTADB
fromInt 27  = SQLITE_NOTICE
fromInt 28  = SQLITE_WARNING
fromInt 100 = SQLITE_ROW
fromInt 101 = SQLITE_DONE
fromInt _   = Unknown

--------------------------------------------------------------------------------
--          Schema
--------------------------------------------------------------------------------

||| Enumeration listing the different types of data that can be stored in
||| an SQLite table column.
public export
data SqlColType : Type where
  BLOB    : SqlColType
  TEXT    : SqlColType
  INTEGER : SqlColType
  REAL    : SqlColType

%runElab derive "SqlColType" [Show,Eq,Ord]

||| Associates an `SqlColType` with the corresponding Idris type.
public export
0 IdrisColType : SqlColType -> Type
IdrisColType BLOB    = Maybe ByteString
IdrisColType TEXT    = Maybe String
IdrisColType INTEGER = Int64
IdrisColType REAL    = Double

||| A database or row schema consistes of the types of data stored in
||| each column.
public export
0 Schema : Type
Schema = List SqlColType

namespace Schema
  ||| A row of data is a heterogeneous list holding values of the types and
  ||| in the order described in the schema.
  public export
  0 Row : Schema -> Type
  Row = All IdrisColType

  ||| A table of data is just a list of rows.
  public export
  0 Table : Schema -> Type
  Table = List . Row

--------------------------------------------------------------------------------
--          Error Type
--------------------------------------------------------------------------------

||| Error type that can occur when interacting with the unsafe world of
||| SQLite.
public export
data SqlError : Type where
  ResultError    : SqlResult -> SqlError
  ColOutOfBounds : (cols, col : Bits32) -> SqlError
  DecodingError  : SqlColType -> String -> SqlError
  NoMoreData     : SqlError

%runElab derive "SqlError" [Show,Eq]

--------------------------------------------------------------------------------
--          Arguments
--------------------------------------------------------------------------------

||| Argument to be bound in an SQL statement.
public export
record Arg where
  constructor A
  name  : String
  type  : SqlColType
  value : IdrisColType type

--------------------------------------------------------------------------------
--          Interface
--------------------------------------------------------------------------------

public export
interface ToSQL (0 a : Type) (0 t : SqlColType) | a where
  toSQL : a -> IdrisColType t

public export
interface FromSQL (0 a : Type) (0 t : SqlColType) | a where
  fromSQL : IdrisColType t -> Either SqlError a
