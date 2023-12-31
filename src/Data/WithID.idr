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

%runElab derive "WithID" [Show,Eq,Ord,ToRow,FromRow]
