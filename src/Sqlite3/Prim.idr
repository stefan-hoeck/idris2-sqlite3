||| This was taken mostly verbatim from MarcelineVQ's unfinished idris-sqlite3
||| project: [https://github.com/MarcelineVQ/idris-sqlite3]
module Sqlite3.Prim

import Data.Buffer
import Data.Buffer.Indexed
import Data.List.Quantifiers
import Sqlite3.Types

%default total

export
idris_sqlite : String -> String
idris_sqlite fn = "C:" ++ fn ++ ",libsqlite3-idris"

--------------------------------------------------------------------------------
--          Pointers
--------------------------------------------------------------------------------

export
data DBPtr : Type where

public export
record DB where
  [noHints]
  constructor D
  db : Ptr DBPtr

export
data StmtPtr : Type where

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

%foreign (idris_sqlite "sqlite3_column_int")
prim__sqlite3_column_int32 : Ptr StmtPtr -> (iCol : Bits32) -> PrimIO Int32

%foreign (idris_sqlite "sqlite3_column_int64")
prim__sqlite3_column_int64 : Ptr StmtPtr -> (iCol : Bits32) -> PrimIO Int64

%foreign (idris_sqlite "sqlite3_column_bytes")
prim__sqlite3_column_bytes : Ptr StmtPtr -> (iCol : Bits32) -> PrimIO Bits32

%foreign (idris_sqlite "sqlite3_column_count")
prim__sqlite3_column_count : Ptr StmtPtr -> PrimIO Bits32

--------------------------------------------------------------------------------
--          Pointers
--------------------------------------------------------------------------------

export %inline
ptrFree : Ptr t -> IO ()
ptrFree ptr = primIO $ prim__ptr_free $ prim__forgetPtr ptr

export %inline
newAnyPtr : IO AnyPtr
newAnyPtr = primIO prim__newptr

export %inline
newPtr : IO (Ptr t)
newPtr = prim__castPtr <$> newAnyPtr

export %inline
dereference : Ptr (Ptr t) -> IO (Ptr t)
dereference ptr =
  prim__castPtr <$> (primIO $ prim__deref (believe_me ptr))

export %inline
strToPtr : String -> IO (Ptr String)
strToPtr = primIO . prim__mkString

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

export %inline
ptrToStr : Ptr String -> IO String
ptrToStr = primIO . prim__getString

--------------------------------------------------------------------------------
--          Status and Errors
--------------------------------------------------------------------------------

export %inline
sqlite3ErrMsg : (d : DB) => IO String
sqlite3ErrMsg = primIO $ prim__sqlite3_errmsg d.db

--------------------------------------------------------------------------------
--          Accessing Columns
--------------------------------------------------------------------------------

export %inline
sqlite3ColumnCount : (s : Stmt) => IO Bits32
sqlite3ColumnCount = primIO $ prim__sqlite3_column_count s.stmt

export
sqlite3ColumnText : (s : Stmt) => (iCol : Bits32) -> IO (Maybe String)
sqlite3ColumnText iCol = do
  ptr <- primIO $ prim__sqlite3_column_text s.stmt iCol
  case prim__nullPtr ptr of
    0 => Just <$> ptrToStr ptr
    _ => pure Nothing

export
sqlite3ColumnBlob : (s : Stmt) => (iCol : Bits32) -> IO (Maybe ByteString)
sqlite3ColumnBlob iCol = do
  ptr <- primIO $ prim__sqlite3_column_blob s.stmt iCol
  case prim__nullAnyPtr ptr of
    0 => do
      n        <- primIO $ prim__sqlite3_column_bytes s.stmt iCol
      Just buf <- newBuffer (cast n) | Nothing => pure Nothing 
      primIO $ prim__copy_buffer n buf ptr 
      pure . Just $ unsafeByteString (cast n) buf
    _ => pure Nothing

export %inline
sqlite3ColumnDouble : (s : Stmt) => (iCol : Bits32) -> IO Double
sqlite3ColumnDouble = primIO . prim__sqlite3_column_double s.stmt

export %inline
sqlite3ColumnInt32 : (s : Stmt) => (iCol : Bits32) -> IO Int32
sqlite3ColumnInt32 = primIO . prim__sqlite3_column_int32 s.stmt

export %inline
sqlite3ColumnInt64 : (s : Stmt) => (iCol : Bits32) -> IO Int64
sqlite3ColumnInt64 = primIO . prim__sqlite3_column_int64 s.stmt

--------------------------------------------------------------------------------
--          Working with Connections
--------------------------------------------------------------------------------

export
sqliteOpen : String -> IO (Either SqlError DB)
sqliteOpen fn = withPtrAlloc $ \db_ptr => do
    res <- fromInt <$> primIO (prim__sqlite_open fn db_ptr)
    case res of
      SQLITE_OK => Right . D <$> dereference db_ptr
      r         => pure (Left $ ResultError r)

export
sqliteClose : DB -> IO SqlResult
sqliteClose d = fromInt <$> primIO (prim__sqlite_close d.db)

export %inline
sqliteClose' : DB -> IO ()
sqliteClose' = ignore . sqliteClose

--------------------------------------------------------------------------------
--          Working with Statements
--------------------------------------------------------------------------------

export
sqliteFinalize : Stmt -> IO SqlResult
sqliteFinalize s = fromInt <$> primIO (prim__sqlite3_finalize s.stmt)

export %inline
sqliteFinalize' : Stmt -> IO ()
sqliteFinalize' = ignore . sqliteFinalize

export
sqlitePrepare : (d : DB) => String -> IO (Either SqlError Stmt)
sqlitePrepare s = withPtrAlloc $ \stmt_ptr => do
    res <- fromInt <$> primIO (prim__sqlite_prepare d.db s (-1) stmt_ptr nullPtr)
    case res of
      SQLITE_OK => Right . S <$> dereference stmt_ptr
      r         => pure (Left $ ResultError r)

export
sqliteStep : Stmt -> IO SqlResult
sqliteStep s = do
    res <- fromInt <$> primIO (prim__sqlite_step s.stmt)
    pure res

--------------------------------------------------------------------------------
--          Loading Rows
--------------------------------------------------------------------------------

loadCol : Stmt => (t : SqlColType) -> Bits32 -> IO (IdrisColType t)
loadCol BLOB    = sqlite3ColumnBlob
loadCol TEXT    = sqlite3ColumnText
loadCol INT     = sqlite3ColumnInt32
loadCol INTEGER = sqlite3ColumnInt64
loadCol REAL    = sqlite3ColumnDouble

export
loadRow : (s : Stmt) => {ts : Schema} -> IO (Either SqlError $ Row ts)
loadRow = do
  ncols <- sqlite3ColumnCount
  go ncols 0 ts
  where
    go : (ncols, col: Bits32) -> (ss : Schema) -> IO (Either SqlError $ Row ss)
    go ncols col []     = pure $ Right []
    go ncols col (h::t) = case col < ncols of
      False => pure $ Left (ColOutOfBounds ncols col)
      True  => do
        v        <- loadCol h col
        Right vs <- go ncols (col+1) t | Left err => pure (Left err)
        pure $ Right (v::vs)

export
loadRows : (s : Stmt) => {ts : _} -> (max : Nat) -> IO (Either SqlError $ Table ts)
loadRows 0 = pure $ Right []
loadRows (S k) = do
  SQLITE_ROW <- sqliteStep s
    | SQLITE_DONE => pure (Right [])
    | res         => pure (Left $ ResultError res)
  Right r  <- loadRow    | Left err => pure (Left err)
  Right rs <- loadRows k | Left err => pure (Left err)
  pure $ Right (r::rs)
