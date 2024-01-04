module Sqlite3.Marshall

import Data.Bits
import Data.Buffer.Indexed
import Data.ByteString
import Data.String
import Data.Vect
import Sqlite3.Types
import public Data.List.Quantifiers.Extra

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

public export
FromCell a => FromRow a where
  fromRowTypes = [FromCellType a]
  fromRow [v]  = fromCell v

||| Utility alias for `fromRowTypes` with an explicit erased type argument.
public export %inline
FromRowTypes : (0 a : Type) -> FromRow a => List SqliteType
FromRowTypes a = fromRowTypes {a}

||| List of SQLite types we require to read a sequence of
||| several values with a `FromRow` implementation.
public export
FromRowsTypes : LAll (FromRow . f) ts -> List SqliteType
FromRowsTypes []        = []
FromRowsTypes (x :: xs) = fromRowTypes @{x} ++ FromRowsTypes xs

fromRowImpl :
     (ps : LAll (FromRow . f) ts)
  -> LAll (Maybe . IdrisType) (FromRowsTypes ps)
  -> Either SqlError (LAll f ts)
fromRowImpl []        [] = Right []
fromRowImpl (p :: ps) xs =
  let (qs,rs)  := splitAt (fromRowTypes @{p}) xs
      Right v  := fromRow @{p} qs   | Left err => Left err
      Right vs := fromRowImpl ps rs | Left err => Left err
   in Right (v::vs)

public export
{0 f : k -> Type} -> (ps : LAll (FromRow . f) ts) => FromRow (LAll f ts) where
  fromRowTypes = FromRowsTypes ps
  fromRow      = fromRowImpl ps

||| We can also convert a row of values to a vector of values.
|||
||| This function computes the types we need.
public export
FromRowsTypesN : Nat -> FromRow a -> List SqliteType
FromRowsTypesN 0     p = []
FromRowsTypesN (S k) p = fromRowTypes @{p} ++ FromRowsTypesN k p

fromRowVectImpl :
     (n : Nat)
  -> (p : FromRow a)
  -> LAll (Maybe . IdrisType) (FromRowsTypesN n p)
  -> Either SqlError (Vect n a)
fromRowVectImpl 0     p [] = Right []
fromRowVectImpl (S k) p xs =
  let (qs,rs)  := splitAt (fromRowTypes @{p}) xs
      Right v  := fromRow @{p} qs        | Left err => Left err
      Right vs := fromRowVectImpl k p rs | Left err => Left err
   in Right (v::vs)

public export
{n : _} -> (p : FromRow a) => FromRow (Vect n a) where
  fromRowTypes = FromRowsTypesN n p
  fromRow      = fromRowVectImpl n p

--------------------------------------------------------------------------------
-- ToRow
--------------------------------------------------------------------------------

||| Inteface for converting an Idris value from a row in a table.
public export
interface ToRow a where
  constructor MkToRow
  toRowTypes : List SqliteType
  toRow      : a -> LAll (Maybe . IdrisType) toRowTypes

public export
ToCell a => ToRow a where
  toRowTypes = [ToCellType a]
  toRow v    = [toCell v]

||| Utility alias for `fromRowTypes` with an explicit erased type argument.
public export %inline
ToRowTypes : (0 a : Type) -> ToRow a => List SqliteType
ToRowTypes a = toRowTypes {a}

||| List of SQLite types we require to read a sequence of
||| several values with a `ToRow` implementation.
public export
ToRowsTypes : LAll (ToRow . f) ts -> List SqliteType
ToRowsTypes []        = []
ToRowsTypes (p :: ps) = toRowTypes @{p} ++ ToRowsTypes ps

toRowImpl :
     (ps : LAll (ToRow . f) ts)
  -> LAll f ts
  -> LAll (Maybe . IdrisType) (ToRowsTypes ps)
toRowImpl []      []      = []
toRowImpl (p::ps) (v::vs) = toRow v ++ toRowImpl ps vs

public export
{0 f : k -> Type} -> (ps : LAll (ToRow . f) ts) => ToRow (LAll f ts) where
  toRowTypes = ToRowsTypes ps
  toRow      = toRowImpl ps

||| We can also convert a vector of values to a (flat) row of values.
|||
||| This function computes the types we need.
public export
ToRowsTypesN : Nat -> ToRow a -> List SqliteType
ToRowsTypesN 0     p = []
ToRowsTypesN (S k) p = toRowTypes @{p} ++ ToRowsTypesN k p

toRowVectImpl :
     (n : Nat)
  -> (p : ToRow a)
  -> Vect n a
  -> LAll (Maybe . IdrisType) (ToRowsTypesN n p)
toRowVectImpl 0     p []      = []
toRowVectImpl (S k) p (v::vs) = toRow @{p} v ++ toRowVectImpl k p vs

public export
{n : _} -> (p : ToRow a) => ToRow (Vect n a) where
  toRowTypes = ToRowsTypesN n p
  toRow      = toRowVectImpl n p

||| List concatenation specialized to behave nicely during unification.
public export
Concat : List (List a) -> List a
Concat []        = []
Concat (x :: xs) = x ++ Concat xs

||| Generalization of `Data.List.Quantifiers.splitAt`, to split a
||| heterogeneous list based on a list of lists of its indices.
public export
splitAll : (v : List (List a)) -> LAll f (Concat v) -> LAll (LAll f) v
splitAll []          [] = []
splitAll (xs :: xss) ys =
  let (zs,r) := splitAt xs ys
   in zs :: splitAll xss r
