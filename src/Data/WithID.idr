module Data.WithID

import Data.List.Quantifiers
import Derive.Sqlite3

%default total
%language ElabReflection

public export
record WithID (a : Type) where
  constructor MkWithID
  id    : Bits32
  value : a

%runElab derive "WithID" [Show,Eq,Ord]

public export
AsRow a => AsRow (WithID a) where
  rowTypes = INTEGER :: RowTypes a
  toRow (MkWithID i v) = toCell i :: toRow v
  fromRow (h::t) = [| MkWithID (fromCell h) (fromRow t) |]
