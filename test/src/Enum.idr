module Enum

import Derive.Sqlite3

%default total
%language ElabReflection

public export
data MolType = Compound | Mixture | Polymer | Enzyme

%runElab derive "MolType" [Show, Eq, Ord, AsCell]
