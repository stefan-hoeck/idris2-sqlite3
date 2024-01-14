||| This was taken mostly verbatim from MarcelineVQ's unfinished idris-sqlite3
||| project: [https://github.com/MarcelineVQ/idris-sqlite3]
module Sqlite3.Prim

import Data.Buffer
import Data.ByteString
import Data.List.Quantifiers

import Sqlite3.Expr
import Sqlite3.Marshall
import Sqlite3.Parameter
import Sqlite3.Types

%default total

export
idris_sqlite : String -> String
idris_sqlite fn = "C:" ++ fn ++ ",libsqlite3-idris"

--------------------------------------------------------------------------------
--          Pointers
--------------------------------------------------------------------------------

||| Database pointer tag.
export
data DBPtr : Type where

||| Pointer to an sqlite3 database.
public export
record DB where
  [noHints]
  constructor D
  db : Ptr DBPtr

||| SQL statement pointer tag.
export
data StmtPtr : Type where

||| Pointer to an SQL statement
public export
record Stmt where
  [noHints]
  constructor S
  stmt : Ptr StmtPtr

--------------------------------------------------------------------------------
--          FFI
--------------------------------------------------------------------------------

%foreign (idris_sqlite "newptr")
prim__newptr : PrimIO AnyPtr

%foreign (idris_sqlite "ptr_free")
prim__ptr_free : AnyPtr -> PrimIO ()

%foreign (idris_sqlite "null")
prim__null : AnyPtr

%foreign (idris_sqlite "deref")
prim__deref : Ptr AnyPtr -> PrimIO AnyPtr

%foreign (idris_sqlite "mkString")
prim__mkString : String -> PrimIO (Ptr String)

%foreign (idris_sqlite "getString")
prim__getString : Ptr String -> PrimIO String

%foreign (idris_sqlite "copy_buffer")
prim__copy_buffer : Bits32 -> Buffer -> AnyPtr -> PrimIO ()

%foreign (idris_sqlite "sqlver")
prim__sqlite_ver : PrimIO ()

%foreign (idris_sqlite "sqlite3_free")
prim__sqlite3_free : AnyPtr -> PrimIO AnyPtr

%foreign (idris_sqlite "sqlite3_malloc")
prim__sqlite3_malloc : Int -> PrimIO AnyPtr

%foreign (idris_sqlite "sqlite3_open")
prim__sqlite_open : String -> Ptr (Ptr DBPtr) -> PrimIO Int

-- open takes an sqlite3**
%foreign (idris_sqlite "sqlite3_open_v2")
prim__sqlite_open_v2 : String -> Ptr (Ptr DBPtr) -> Int -> String -> PrimIO Int

%foreign (idris_sqlite "sqlite3_close")
prim__sqlite_close : Ptr DBPtr -> PrimIO Int

%foreign (idris_sqlite "sqlite3_prepare_v2")
prim__sqlite_prepare : Ptr DBPtr -> String -> Int -> Ptr (Ptr StmtPtr) -> Ptr (Ptr String) -> PrimIO Int

%foreign (idris_sqlite "sqlite3_step")
prim__sqlite_step : Ptr StmtPtr -> PrimIO Int

%foreign (idris_sqlite "sqlite3_exec")
prim__sqlite_exec : Ptr DBPtr -> String -> AnyPtr -> AnyPtr -> Ptr String -> PrimIO Int

%foreign (idris_sqlite "sqlite3_finalize")
prim__sqlite3_finalize : Ptr StmtPtr -> PrimIO Int

%foreign (idris_sqlite "sqlite3_extended_errcode")
prim__sqlite3_extended_errcode : Ptr DBPtr -> PrimIO Int

%foreign (idris_sqlite "sqlite3_errmsg")
prim__sqlite3_errmsg : Ptr DBPtr -> PrimIO String

%foreign (idris_sqlite "sqlite3_errstr")
prim__sqlite3_errstr : Int -> PrimIO String

%foreign (idris_sqlite "sqlite3_column_text")
prim__sqlite3_column_text : Ptr StmtPtr -> (iCol : Bits32) -> PrimIO (Ptr String)

%foreign (idris_sqlite "sqlite3_column_blob")
prim__sqlite3_column_blob : Ptr StmtPtr -> (iCol : Bits32) -> PrimIO AnyPtr

%foreign (idris_sqlite "sqlite3_column_double")
prim__sqlite3_column_double : Ptr StmtPtr -> (iCol : Bits32) -> PrimIO Double

%foreign (idris_sqlite "sqlite3_column_type")
prim__sqlite3_column_type : Ptr StmtPtr -> (iCol : Bits32) -> PrimIO Bits8

%foreign (idris_sqlite "sqlite3_column_int")
prim__sqlite3_column_int32 : Ptr StmtPtr -> (iCol : Bits32) -> PrimIO Int32

%foreign (idris_sqlite "sqlite3_column_int64")
prim__sqlite3_column_int64 : Ptr StmtPtr -> (iCol : Bits32) -> PrimIO Int64

%foreign (idris_sqlite "sqlite3_column_bytes")
prim__sqlite3_column_bytes : Ptr StmtPtr -> (iCol : Bits32) -> PrimIO Bits32

%foreign (idris_sqlite "sqlite3_column_count")
prim__sqlite3_column_count : Ptr StmtPtr -> PrimIO Bits32

%foreign (idris_sqlite "bind_text")
prim__sqlite3_bind_text : Ptr StmtPtr -> (ix : Bits32) -> String -> PrimIO Int

%foreign (idris_sqlite "bind_buffer")
prim__sqlite3_bind_blob : Ptr StmtPtr -> (ix : Bits32) -> Buffer ->  (size : Bits32) -> PrimIO Int

%foreign (idris_sqlite "sqlite3_bind_double")
prim__sqlite3_bind_double : Ptr StmtPtr -> (ix : Bits32) -> Double -> PrimIO Int

%foreign (idris_sqlite "sqlite3_bind_int")
prim__sqlite3_bind_int32 : Ptr StmtPtr -> (ix : Bits32) -> Int32 -> PrimIO Int

%foreign (idris_sqlite "sqlite3_bind_int64")
prim__sqlite3_bind_int64 : Ptr StmtPtr -> (ix : Bits32) -> Int64 -> PrimIO Int

%foreign (idris_sqlite "sqlite3_bind_parameter_index")
prim__sqlite3_bind_parameter_index : Ptr StmtPtr -> String -> PrimIO Bits32

%foreign (idris_sqlite "sqlite3_bind_null")
prim__sqlite3_bind_null : Ptr StmtPtr -> (ix : Bits32) -> PrimIO Int

--------------------------------------------------------------------------------
--          Pointers
--------------------------------------------------------------------------------

||| Free the given pointer.
export %inline
ptrFree : Ptr t -> IO ()
ptrFree ptr = primIO $ prim__ptr_free $ prim__forgetPtr ptr

||| Allocate a new pointer.
export %inline
newAnyPtr : IO AnyPtr
newAnyPtr = primIO prim__newptr

||| Allocate a new tagged pointer.
export %inline
newPtr : IO (Ptr t)
newPtr = prim__castPtr <$> newAnyPtr

||| Dereference the given pointer.
export %inline
dereference : Ptr (Ptr t) -> IO (Ptr t)
dereference ptr =
  prim__castPtr <$> (primIO $ prim__deref (believe_me ptr))

export %inline
strToPtr : String -> IO (Ptr String)
strToPtr = primIO . prim__mkString

||| The `null` pointer
export %inline
nullPtr : Ptr t
nullPtr = prim__castPtr $ prim__null

||| Act with a pointer and free it afterwards
export
withPtr : Ptr a -> (Ptr a -> IO b) -> IO b
withPtr ptr act = act ptr <* ptrFree ptr

||| Allocate a pointer, act upon it, and free it afterwards
export
withPtrAlloc : (Ptr a -> IO b) -> IO b
withPtrAlloc act = newPtr >>= (`withPtr` act)

||| Convert an FFI string to an Idris string.
export %inline
ptrToStr : Ptr String -> IO String
ptrToStr = primIO . prim__getString

--------------------------------------------------------------------------------
--          Status and Errors
--------------------------------------------------------------------------------

||| Get the current error message.
export %inline
sqlite3ErrMsg : (d : DB) => IO String
sqlite3ErrMsg = primIO $ prim__sqlite3_errmsg d.db

export %inline
sqlFailRes : DB => SqlResult -> IO (Either SqlError a)
sqlFailRes r = do
  msg <- sqlite3ErrMsg
  pure (Left $ ResultError r msg)

export %inline
sqlFail : DB => Int -> IO (Either SqlError a)
sqlFail = sqlFailRes . fromInt

--------------------------------------------------------------------------------
--          Accessing Columns
--------------------------------------------------------------------------------

||| Get the current column count for the given statement.
export %inline
sqlite3ColumnCount : (s : Stmt) => IO Bits32
sqlite3ColumnCount = primIO $ prim__sqlite3_column_count s.stmt

||| Try to read the text stored in the current column.
|||
||| Note: This assumes that callers have already verified that the
|||       stored value is not a null pointer, for instance, by first
|||       invoking prim__sqlite3_column_type
export %inline
sqlite3ColumnText : (s : Stmt) => (iCol : Bits32) -> IO String
sqlite3ColumnText iCol = do
  ptr <- primIO $ prim__sqlite3_column_text s.stmt iCol
  ptrToStr ptr

||| Try to read the bytestring stored in the current column.
|||
||| Note: This assumes that callers have already verified that the
|||       stored value is not a null pointer, for instance, by first
|||       invoking prim__sqlite3_column_type
export
sqlite3ColumnBlob : (s : Stmt) => (iCol : Bits32) -> IO ByteString
sqlite3ColumnBlob iCol = do
  ptr <- primIO $ prim__sqlite3_column_blob s.stmt iCol
  n        <- primIO $ prim__sqlite3_column_bytes s.stmt iCol
  Just buf <- newBuffer (cast n) | Nothing => pure empty
  primIO $ prim__copy_buffer n buf ptr
  pure $ unsafeByteString (cast n) buf

||| Read the floating point number stored in the current column.
export %inline
sqlite3ColumnDouble : (s : Stmt) => (iCol : Bits32) -> IO Double
sqlite3ColumnDouble = primIO . prim__sqlite3_column_double s.stmt

||| Read the 32bit integer stored in the current column.
export %inline
sqlite3ColumnInt32 : (s : Stmt) => (iCol : Bits32) -> IO Int32
sqlite3ColumnInt32 = primIO . prim__sqlite3_column_int32 s.stmt

||| Read the 64bit integer stored in the current column.
export %inline
sqlite3ColumnInt64 : (s : Stmt) => (iCol : Bits32) -> IO Int64
sqlite3ColumnInt64 = primIO . prim__sqlite3_column_int64 s.stmt

--------------------------------------------------------------------------------
--          Working with Connections
--------------------------------------------------------------------------------

||| Tries to open a connection to the given database.
|||
||| `path` is typically a relative or absolute path on the file system
||| pointing to the database we want to work on. If `path` equals `":memory:"`,
||| a temporary in-memory database will be created until the connection is
||| closed. If `path` is the empty string, a temporary on-disk database will be
||| created, which will be deleted once the connection is closed.
export
sqliteOpen : (path : String) -> IO (Either SqlError DB)
sqliteOpen fn = withPtrAlloc $ \db_ptr => do
    0 <- primIO (prim__sqlite_open fn db_ptr)
      | r => pure (Left $ ResultError (fromInt r) "unable to open connect to \{fn}")
    Right . D <$> dereference db_ptr

||| Closes the given database connection returning an `SqlResult` describing
||| if all went well.
export %inline
sqliteClose : DB -> IO SqlResult
sqliteClose d = fromInt <$> primIO (prim__sqlite_close d.db)

||| Convenience alias for `ingore . sqliteClose`.
export %inline
sqliteClose' : DB -> IO ()
sqliteClose' = ignore . sqliteClose

--------------------------------------------------------------------------------
--          Working with Statements
--------------------------------------------------------------------------------

||| Deletes a prepared SQL statement.
|||
||| This can be called on a statement at any time, even if there is still
||| more data available or the statement has not been evaluated at all.
export %inline
sqliteFinalize : Stmt -> IO SqlResult
sqliteFinalize s = fromInt <$> primIO (prim__sqlite3_finalize s.stmt)

||| Convenience alias for `ignore . sqliteFinalize`.
export %inline
sqliteFinalize' : Stmt -> IO ()
sqliteFinalize' = ignore . sqliteFinalize

||| Prepares an SQL statement for execution with the given database connection.
export
sqlitePrepare : (d : DB) => String -> IO (Either SqlError Stmt)
sqlitePrepare s = withPtrAlloc $ \stmt_ptr => do
    0 <- primIO (prim__sqlite_prepare d.db s (-1) stmt_ptr nullPtr)
      | r => sqlFail r
    Right . S <$> dereference stmt_ptr

bindParam : DB => Stmt -> Parameter -> IO Int
bindParam s (P n t v) = do
  ix <- fromPrim $ prim__sqlite3_bind_parameter_index s.stmt n
  case t of
    INTEGER => fromPrim $ prim__sqlite3_bind_int64 s.stmt ix v
    REAL    => fromPrim $ prim__sqlite3_bind_double s.stmt ix v
    TEXT    => fromPrim $ prim__sqlite3_bind_text s.stmt ix v
    BLOB    => do
      buf <- toBuffer v
      fromPrim $ prim__sqlite3_bind_blob s.stmt ix buf (cast v.size)

bindParams : DB => Stmt -> List Parameter -> IO Int
bindParams stmt []        = pure 0
bindParams stmt (x :: xs) = do
  0 <- bindParam stmt x | x => pure x
  bindParams stmt xs

||| Binds the given parameters to an SQLite statement.
export
sqliteBind : (d : DB) => (s : Stmt) => List Parameter -> IO (Either SqlError ())
sqliteBind xs = do
  0 <- bindParams s xs | r => sqlFail r
  pure (Right ())

||| Evaluates the given prepared SQL statement.
|||
||| Some of the possible results
|||  * `SQLITE_DONE`   : Execution has finished and there is no more data
|||  * `SQLITE_ROW`    : Another row of output is available
|||  * `SQLITE_MISUSE` : Invalid use of statement (perhaps it was already finalized?)
|||  * `SQLITE_BUSY`   : If the statement is a commit you can retry it
|||
||| More details about the possible results can be found at the
||| [documentation of the SQLite C interface](https://www.sqlite.org/c3ref/step.html).
export %inline
sqliteStep : Stmt -> IO SqlResult
sqliteStep s = fromInt <$> primIO (prim__sqlite_step s.stmt)

--------------------------------------------------------------------------------
--          Loading Rows
--------------------------------------------------------------------------------

tryLoad :
     {auto s : Stmt}
  -> Bits8
  -> (t : SqliteType)
  -> Bits32
  -> IO (Either SqlError $ Maybe (IdrisType t))
tryLoad 1 INTEGER ix = Right . Just <$> sqlite3ColumnInt64 ix
tryLoad 2 REAL    ix = Right . Just <$> sqlite3ColumnDouble ix
tryLoad 3 TEXT    ix = Right . Just <$> sqlite3ColumnText ix
tryLoad 4 BLOB    ix = Right . Just <$> sqlite3ColumnBlob ix
tryLoad 5 _       ix = pure (Right Nothing)
tryLoad 1 t       ix = pure $ Left (TypeMismatch t INTEGER)
tryLoad 2 t       ix = pure $ Left (TypeMismatch t REAL)
tryLoad 3 t       ix = pure $ Left (TypeMismatch t TEXT)
tryLoad _ t       ix = pure $ Left (TypeMismatch t BLOB)

loadCell :
     {t : _}
  -> {auto s : Stmt}
  -> Bits32
  -> IO (Either SqlError $ Maybe (IdrisType t))
loadCell ix = do
  tpe     <- fromPrim $ prim__sqlite3_column_type s.stmt ix
  tryLoad tpe t ix

||| Tries to read a single row of data from an SQL statement.
|||
||| Only invoke this utility after `sqliteStep` returned with result
||| `SQLITE_ROW`.
export
loadRow :
     {ts     : List SqliteType}
  -> {auto s : Stmt}
  -> IO (Either SqlError $ All (Maybe . IdrisType) ts)
loadRow = do
  ncols <- sqlite3ColumnCount
  go (cast ncols) 0 ts

  where
    go :
         Nat
      -> Bits32
      -> (ss : List SqliteType)
      -> IO (Either SqlError $ All (Maybe . IdrisType) ss)
    go _     _   []      = pure (Right [])
    go 0     _   (t::ss) = pure (Left NoMoreData)
    go (S k) col (t::ss) = do
      Right c  <- loadCell col     | Left err => pure (Left err)
      Right cs <- go k (col +1) ss | Left err => pure (Left err)
      pure (Right $ c :: cs)

||| Tries to extract up to `max` lines of data from a prepared SQL statement.
export
loadRows :
     {auto db : DB}
  -> {auto s  : Stmt}
  -> {auto ps : FromRow a}
  -> (max     : Nat)
  -> IO (Either SqlError $ List a)
loadRows = go [<]
  where
    go : SnocList a -> Nat -> IO (Either SqlError $ List a)
    go sr 0     = pure (Right $ sr <>> [])
    go sr (S k) = do
      SQLITE_ROW <- sqliteStep s
        | SQLITE_DONE => pure (Right $ sr <>> [])
        | res         => sqlFailRes res
      Right r  <- loadRow | Left err => pure (Left err)
      case fromRow {a} r of
        Right v  => go (sr :< v) k
        Left err => pure (Left err)
