module Sqlite3.Marshall

import Data.Buffer.Indexed
import Data.List.Quantifiers
import Sqlite3.Types

%default total

public export
interface AsCell a where
  cellType : SqliteType
  toCell   : a -> Maybe (IdrisType cellType)
  fromCell : Maybe (IdrisType cellType) -> Either SqlError a

public export %inline
CellType : (0 a : Type) -> AsCell a => SqliteType
CellType a = cellType {a}

export
decodeJust :
     String
  -> (t -> Either SqlError a)
  -> Maybe t
  -> Either SqlError a
decodeJust str f Nothing  = Left (NullPointer str)
decodeJust str f (Just v) = f v

public export %inline
AsCell a => AsCell (Maybe a) where
  cellType          = CellType a
  toCell m          = m >>= toCell
  fromCell Nothing  = Right Nothing
  fromCell v        = Just <$> fromCell v

public export
AsCell Int64 where
  cellType = INTEGER
  toCell   = Just
  fromCell = decodeJust "Int64" Right

public export
AsCell Int32 where
  cellType = INTEGER
  toCell   = Just . cast
  fromCell = decodeJust "Int32" (Right . cast)

public export
AsCell Int16 where
  cellType = INTEGER
  toCell   = Just . cast
  fromCell = decodeJust "Int16" (Right . cast)

public export
AsCell Int8 where
  cellType = INTEGER
  toCell   = Just . cast
  fromCell = decodeJust "Int8" (Right . cast)

public export
AsCell Bits64 where
  cellType = INTEGER
  toCell   = Just . cast
  fromCell = decodeJust "Bits64" (Right . cast)

public export
AsCell Bits32 where
  cellType = INTEGER
  toCell   = Just . cast
  fromCell = decodeJust "Bits32" (Right . cast)

public export
AsCell Bits16 where
  cellType = INTEGER
  toCell   = Just . cast
  fromCell = decodeJust "Bits16" (Right . cast)

public export
AsCell Bits8 where
  cellType = INTEGER
  toCell   = Just . cast
  fromCell = decodeJust "Bits8" (Right . cast)

public export
AsCell String where
  cellType = TEXT
  toCell   = Just
  fromCell = decodeJust "String" Right

public export
AsCell ByteString where
  cellType = BLOB
  toCell   = Just
  fromCell = decodeJust "ByteString" Right

public export
AsCell Double where
  cellType = REAL
  toCell   = Just
  fromCell = decodeJust "Double" Right

public export
AsCell Bool where
  cellType     = INTEGER
  toCell True  = Just 1
  toCell False = Just 0
  fromCell     = decodeJust "Bool" (\case 0 => Right False; _ => Right True)
