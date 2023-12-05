module Schema

import Data.WithID
import Data.Buffer.Indexed
import Data.ByteString
import Data.List.Quantifiers
import Derive.Sqlite3
import Enum

%default total
%language ElabReflection

public export
Units : SQLTable
Units =
  table "units"
  [ C "unit_id" INTEGER
  , C "name"    TEXT
  , C "head"    INTEGER
  ]

public export
Employees : SQLTable
Employees =
  table "employees"
    [ C "employee_id" INTEGER
    , C "name"        TEXT
    , C "salary"      REAL
    , C "unit_id"     INTEGER
    ]

public export
Molecules : SQLTable
Molecules =
  table "molecules"
    [ C "molecule_id" INTEGER
    , C "name"        TEXT
    , C "casnr"       TEXT
    , C "molweight"   REAL
    , C "type"        TEXT
    ]

public export
Files : SQLTable
Files =
  table "files"
    [ C "file_id" INTEGER
    , C "content" BLOB
    ]

public export
Edges : SQLTable
Edges =
  table "edges"
    [ C "u" TEXT
    , C "v" TEXT
    ]

export
createMolecules : Cmd TCreate
createMolecules =
  CREATE_TABLE Molecules
    [ PRIMARY_KEY ["molecule_id"]
    , AUTOINCREMENT "molecule_id"
    , NOT_NULL "name"
    , NOT_NULL "type"
    , UNIQUE ["name"]
    ]

export
createFiles : Cmd TCreate
createFiles =
  CREATE_TABLE Files
    [ PRIMARY_KEY ["file_id"]
    , AUTOINCREMENT "file_id"
    , NOT_NULL "content"
    ]

export
createUnits : Cmd TCreate
createUnits =
  CREATE_TABLE Units
    [ PRIMARY_KEY ["unit_id"]
    , AUTOINCREMENT "unit_id"
    , FOREIGN_KEY Employees ["head"] ["employee_id"]
    , NOT_NULL "name"
    , UNIQUE ["name"]
    ]

export
createEmployees : Cmd TCreate
createEmployees =
  CREATE_TABLE Employees
    [ PRIMARY_KEY ["employee_id"]
    , AUTOINCREMENT "employee_id"
    , FOREIGN_KEY Units ["unit_id"] ["unit_id"]
    , NOT_NULL "name"
    , NOT_NULL "salary"
    , NOT_NULL "unit_id"
    , UNIQUE ["name"]
    ]

export
createEdges : Cmd TCreate
createEdges =
  CREATE_TABLE Edges
    [ PRIMARY_KEY ["u", "v"]
    , NOT_NULL "u"
    , NOT_NULL "v"
    ]

--------------------------------------------------------------------------------
-- Idris Types
--------------------------------------------------------------------------------

public export
record Salary where
  constructor S
  value : Double

%runElab derive "Schema.Salary" [Show, Eq, Ord, FromDouble, ToCell, FromCell]

public export
record OrgUnit (h : Type) where
  constructor U
  name : String
  head : h

%runElab derive "Schema.OrgUnit" [Show, Eq, ToRow, FromRow]

public export
record Employee (u : Type) where
  constructor E
  name   : String
  salary : Salary
  unit   : u

%runElab derive "Schema.Employee" [Show, Eq, ToRow, FromRow]

public export
record Molecule where
  constructor M
  name      : String
  casNr     : Maybe String
  molWeight : Maybe Double
  type      : MolType

%runElab derive "Molecule" [Show, Eq, ToRow, FromRow]

public export
record File where
  constructor F
  content : ByteString

%runElab derive "File" [Show, Eq, ToRow, FromRow]

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

export
insertEdge : String -> String -> Cmd TInsert
insertEdge u v = INSERT Edges ["u", "v"] [val u, val v]

--------------------------------------------------------------------------------
-- Query
--------------------------------------------------------------------------------

public export
mol : Expr [<Molecules] BOOL -> Query (WithID Molecule)
mol x =
  SELECT
    ["molecule_id", "name", "casnr", "molweight", "type"]
    [<FROM Molecules]
  `WHERE` x

public export
file : Expr [<Files] BOOL -> Query (WithID File)
file x = SELECT ["file_id", "content"] [<FROM Files] `WHERE` x

public export
employee : Query (WithID $ Employee String)
employee =
  SELECT
    ["e.employee_id", "e.name", "e.salary", "u.name"]
    [< FROM (Employees `AS` "e")
    ,  JOIN (Units `AS` "u") `USING` ["unit_id"]
    ]
  `WHERE`    ("e.salary" > 3000.0)
  `ORDER_BY` [ASC "e.salary", ASC "e.name"]

public export
unitStats : LQuery [String,Bits32,Salary,Salary,Salary]
unitStats =
  SELECT
    [ "u.name"
    ,  COUNT "e.name" `AS` "num_employees"
    ,  AVG "e.salary" `AS` "average_salary"
    ,  MIN "e.salary" `AS` "min_salary"
    ,  MAX "e.salary" `AS` "max_salary"
    ]
    [< FROM (Employees `AS` "e")
    ,  JOIN (Units `AS` "u") `USING` ["unit_id"]
    ]
    `GROUP_BY` ["e.unit_id"]
    `HAVING`   ("num_employees" > 3)
    `ORDER_BY` [ASC "average_salary"]

public export
heads : Query (OrgUnit String)
heads =
  SELECT
    ["u.name", "e.name"]
    [< FROM $ Employees `AS` "e"
    ,  JOIN (Units `AS` "u") `ON` ("e.employee_id" == "u.head")
    ]

public export
nonHeads : LQuery [Bits32, String]
nonHeads =
  SELECT
    ["e.employee_id", "e.name"]
    [< FROM $ Employees `AS` "e"
    ,  OUTER_JOIN (Units `AS` "u") `ON` ("e.employee_id" == "u.head")
    ]
    `WHERE`    IS NULL "u.head"
    `ORDER_BY` [ASC "e.name"]

public export
tuples : LQuery [String,Double,Double,MolType]
tuples =
  SELECT
    ["e.name", "m1.molweight", "m2.molweight", "m1.type"]
    [< FROM $ Employees `AS` "e"
    ,  CROSS_JOIN $ Molecules `AS` "m1"
    ,  CROSS_JOIN $ Molecules `AS` "m2"
    ]
    `WHERE` ("m1.molweight" < "m2.molweight")
    `OFFSET` 2

public export
parents : LQuery [String]
parents =
  SELECT_DISTINCT
    ["u"]
    [< FROM $ Edges `AS` "e"]
