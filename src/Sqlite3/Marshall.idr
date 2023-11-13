module Sqlite3.Marshall

import Data.Buffer.Indexed
import public Data.List.Quantifiers
import Sqlite3.Types

%default total

public export
0 LAll : (0 f : k -> Type) -> List k -> Type
LAll = Data.List.Quantifiers.All.All

--------------------------------------------------------------------------------
-- FromCell
--------------------------------------------------------------------------------

||| Inteface for converting an Idris value to a single cell in a table row.
public export
interface FromCell a where
  constructor MkFromCell
  fromCellType : SqliteType
  fromCell : Maybe (IdrisType fromCellType) -> Either SqlError a

||| Utility alias for `fromCellType` with an explicit erased type argument.
public export %inline
FromCellType : (0 a : Type) -> FromCell a => SqliteType
FromCellType a = fromCellType {a}

||| Utility for implementing `fromCell` for non-nullable data types.
|||
||| In case of a `Nothing` (corresponding to `NULL` in SQL land),
||| this fails with a `NullPointer` error that wraps the type name
||| to display, what kind of data we tried to convert.
export
decodeJust :
     (type : String)
  -> (t -> Either SqlError a)
  -> Maybe t
  -> Either SqlError a
decodeJust str f Nothing  = Left (NullPointer str)
decodeJust str f (Just v) = f v

public export %inline
FromCell a => FromCell (Maybe a) where
  fromCellType      = FromCellType a
  fromCell Nothing  = Right Nothing
  fromCell v        = Just <$> fromCell v

public export
FromCell Integer where
  fromCellType = INTEGER
  fromCell     = decodeJust "Integer" (Right . cast)

public export
FromCell Nat where
  fromCellType = INTEGER
  fromCell     = decodeJust "Nat" (Right . cast)

public export
FromCell Int64 where
  fromCellType = INTEGER
  fromCell     = decodeJust "Int64" Right

public export
FromCell Int32 where
  fromCellType = INTEGER
  fromCell     = decodeJust "Int32" (Right . cast)

public export
FromCell Int16 where
  fromCellType = INTEGER
  fromCell     = decodeJust "Int16" (Right . cast)

public export
FromCell Int8 where
  fromCellType = INTEGER
  fromCell     = decodeJust "Int8" (Right . cast)

public export
FromCell Bits64 where
  fromCellType = INTEGER
  fromCell     = decodeJust "Bits64" (Right . cast)

public export
FromCell Bits32 where
  fromCellType = INTEGER
  fromCell     = decodeJust "Bits32" (Right . cast)

public export
FromCell Bits16 where
  fromCellType = INTEGER
  fromCell     = decodeJust "Bits16" (Right . cast)

public export
FromCell Bits8 where
  fromCellType = INTEGER
  fromCell     = decodeJust "Bits8" (Right . cast)

public export
FromCell String where
  fromCellType = TEXT
  fromCell     = decodeJust "String" Right

public export
FromCell ByteString where
  fromCellType = BLOB
  fromCell     = decodeJust "ByteString" Right

public export
FromCell Double where
  fromCellType = REAL
  fromCell     = decodeJust "Double" Right

public export
FromCell Bool where
  fromCellType = INTEGER
  fromCell     = decodeJust "Bool" (\case 0 => Right False; _ => Right True)

--------------------------------------------------------------------------------
-- ToCell
--------------------------------------------------------------------------------

||| Inteface for converting an Idris value to a single cell in a table row.
public export
interface ToCell a where
  constructor MkToCell
  toCellType : SqliteType
  toCell     : a -> Maybe (IdrisType toCellType)

||| Utility alias for `toCellType` with an explicit erased type argument.
public export %inline
ToCellType : (0 a : Type) -> ToCell a => SqliteType
ToCellType a = toCellType {a}

public export %inline
ToCell a => ToCell (Maybe a) where
  toCellType = ToCellType a
  toCell m   = m >>= toCell

public export
ToCell Int64 where
  toCellType = INTEGER
  toCell     = Just

public export
ToCell Int32 where
  toCellType = INTEGER
  toCell     = Just . cast

public export
ToCell Int16 where
  toCellType = INTEGER
  toCell     = Just . cast

public export
ToCell Int8 where
  toCellType = INTEGER
  toCell     = Just . cast

public export
ToCell Bits64 where
  toCellType = INTEGER
  toCell     = Just . cast

public export
ToCell Bits32 where
  toCellType = INTEGER
  toCell     = Just . cast

public export
ToCell Bits16 where
  toCellType = INTEGER
  toCell     = Just . cast

public export
ToCell Bits8 where
  toCellType = INTEGER
  toCell     = Just . cast

public export
ToCell String where
  toCellType = TEXT
  toCell     = Just

public export
ToCell ByteString where
  toCellType = BLOB
  toCell     = Just

public export
ToCell Double where
  toCellType = REAL
  toCell     = Just

public export
ToCell Bool where
  toCellType   = INTEGER
  toCell True  = Just 1
  toCell False = Just 0

--------------------------------------------------------------------------------
-- FromRow
--------------------------------------------------------------------------------

||| Inteface for converting an Idris value from a row in a table.
public export
interface FromRow a where
  constructor MkFromRow
  fromRowTypes : List SqliteType
  fromRow      : LAll (Maybe . IdrisType) fromRowTypes -> Either SqlError a

||| Utility alias for `fromRowTypes` with an explicit erased type argument.
public export %inline
FromRowTypes : (0 a : Type) -> FromRow a => List SqliteType
FromRowTypes a = fromRowTypes {a}

||| Computes a list of SQLite types from a list of `FromCell` implementations.
public export
FromCellTypes : LAll (FromCell . f) ts -> List SqliteType
FromCellTypes []        = []
FromCellTypes (p :: ps) = fromCellType @{p} :: FromCellTypes ps

fromRowImpl :
     (ps : LAll (FromCell . f) ts)
  -> LAll (Maybe . IdrisType) (FromCellTypes ps)
  -> Either SqlError (All f ts)
fromRowImpl []      []      = Right []
fromRowImpl (p::ps) (v::vs) =
  let Right x  := fromCell v        | Left err => Left err
      Right xs := fromRowImpl ps vs | Left err => Left err
   in Right (x::xs)

export
{0 f : k -> Type} -> (ps : LAll (FromCell . f) ts) => FromRow (LAll f ts) where
  fromRowTypes = FromCellTypes ps
  fromRow      = fromRowImpl ps

--------------------------------------------------------------------------------
-- ToRow
--------------------------------------------------------------------------------

||| Inteface for converting an Idris value from a row in a table.
public export
interface ToRow a where
  constructor MkToRow
  toRowTypes : List SqliteType
  toRow      : a -> LAll (Maybe . IdrisType) toRowTypes

||| Utility alias for `fromRowTypes` with an explicit erased type argument.
public export %inline
ToRowTypes : (0 a : Type) -> ToRow a => List SqliteType
ToRowTypes a = toRowTypes {a}

public export
ToCellTypes : LAll (ToCell . f) ts -> List SqliteType
ToCellTypes []        = []
ToCellTypes (p :: ps) = toCellType @{p} :: ToCellTypes ps

toRowImpl :
     (ps : LAll (ToCell . f) ts)
  -> LAll f ts
  -> LAll (Maybe . IdrisType) (ToCellTypes ps)
toRowImpl []      []      = []
toRowImpl (p::ps) (v::vs) = toCell v :: toRowImpl ps vs

export
{0 f : k -> Type} -> (ps : LAll (ToCell . f) ts) => ToRow (LAll f ts) where
  toRowTypes = ToCellTypes ps
  toRow      = toRowImpl ps
