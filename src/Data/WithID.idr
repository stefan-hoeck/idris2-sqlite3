module Data.WithID

import Data.List.Quantifiers
import Derive.Sqlite3

%default total
%language ElabReflection

public export
record WithID (a : Type) where
  constructor MkWithID
  id    : Nat
  value : a

%runElab derive "WithID" [Show,Eq,Ord]

public export
FromRow a => FromRow (WithID a) where
  fromRowTypes   = INTEGER :: FromRowTypes a
  fromRow (h::t) = [| MkWithID (fromCell h) (fromRow t) |]
