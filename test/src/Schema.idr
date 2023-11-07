module Schema

import Data.Buffer.Indexed
import Data.ByteString
import Data.List.Quantifiers
import Derive.Sqlite3
import Enum

%default total
%language ElabReflection

public export
Units : Table
Units =
  T "units"
  [ C "unit_id" INTEGER
  , C "name"    TEXT
  , C "head"    INTEGER
  ]

public export
Employees : Table
Employees =
  T "employees"
    [ C "employee_id" INTEGER
    , C "name"        TEXT
    , C "salary"      REAL
    , C "unit_id"     INTEGER
    ]

public export
Molecules : Table
Molecules =
  T "molecules"
    [ C "molecule_id" INTEGER
    , C "name"        TEXT
    , C "casnr"       TEXT
    , C "molweight"   REAL
    , C "type"        TEXT
    ]

public export
Files : Table
Files =
  T "files"
    [ C "file_id" INTEGER
    , C "content" BLOB
    ]

export
createMolecules : Cmd TCreate
createMolecules =
  createTable Molecules
    [ PrimaryKey ["molecule_id"]
    , AutoIncrement "molecule_id"
    , NotNull "name"
    , NotNull "type"
    , Unique ["name"]
    ]

export
createFiles : Cmd TCreate
createFiles =
  createTable Files
    [ PrimaryKey ["file_id"]
    , AutoIncrement "file_id"
    , NotNull "content"
    ]

export
createUnits : Cmd TCreate
createUnits =
  createTable Units
    [ PrimaryKey ["unit_id"]
    , AutoIncrement "unit_id"
    , ForeignKey Employees ["head"] ["employee_id"]
    , NotNull "name"
    , Unique ["name"]
    ]

export
createEmployees : Cmd TCreate
createEmployees =
  createTable Employees
    [ PrimaryKey ["employee_id"]
    , AutoIncrement "employee_id"
    , ForeignKey Units ["unit_id"] ["unit_id"]
    , NotNull "name"
    , NotNull "salary"
    , NotNull "unit_id"
    , Unique ["name"]
    ]

--------------------------------------------------------------------------------
-- Idris Types
--------------------------------------------------------------------------------

public export
record OrgUnit (h : Type) where
  constructor U
  name : String
  head : h

%runElab derive "Schema.OrgUnit" [Show, Eq, AsRow]

public export
record Employee (u : Type) where
  constructor E
  name   : String
  salary : Double
  unit   : u

%runElab derive "Schema.Employee" [Show, Eq, AsRow]

public export
record Molecule where
  constructor M
  name      : String
  casNr     : Maybe String
  molWeight : Maybe Double
  type      : MolType

%runElab derive "Molecule" [Show, Eq, AsRow]

public export
record File where
  constructor F
  content : ByteString

%runElab derive "File" [Show, Eq, AsRow]

public export
record Item (i : Type) where
  constructor I
  id   : Bits32
  item : i

%runElab derive "Schema.Item" [Show, Eq]

public export
AsRow i => AsRow (Item i) where
  rowTypes = INTEGER :: RowTypes i
  toRow (I id itm) = toCell id :: toRow itm
  fromRow (id::t)  = [| I (fromCell id) (fromRow t) |]

--------------------------------------------------------------------------------
-- Create
--------------------------------------------------------------------------------

export
insertUnit : OrgUnit Bits32 -> Cmd TInsert
insertUnit = insert Units ["name", "head"]

export
insertEmployee : Employee Bits32 -> Cmd TInsert
insertEmployee = insert Employees ["name", "salary", "unit_id"]

export
insertMol : Molecule -> Cmd TInsert
insertMol = insert Molecules ["name", "casnr", "molweight", "type"]

export
insertFile : File -> Cmd TInsert
insertFile = insert Files ["content"]

--------------------------------------------------------------------------------
-- Query
--------------------------------------------------------------------------------

export
mol : Expr [Molecules] BOOL -> Query (Item Molecule)
mol =
  SELECT (Tbl Molecules) ["molecule_id", "name", "casnr", "molweight", "type"]

export
file : Expr [Files] BOOL -> Query (Item File)
file = SELECT (Tbl Files) ["file_id", "content"]

export
employee : Query (Item $ Employee String)
employee =
  SELECT
    (JoinUsing (TblAs Employees "e") (TblAs Units "u") False ["unit_id"])
    [ Col "e" "employee_id"
    , Col "e" "name"
    , Col "e" "salary"
    , Col "u" "name"
    ]
    (Col "e" "salary" > 3000.0)
