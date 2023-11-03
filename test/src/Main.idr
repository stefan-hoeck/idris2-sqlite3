module Main

import Control.RIO.Sqlite3
import Data.Buffer.Indexed
import Data.ByteString
import Data.List.Quantifiers
import Derive.Sqlite3
import Enum

%default total
%language ElabReflection

--------------------------------------------------------------------------------
--          Errors
--------------------------------------------------------------------------------

0 Errs : List Type
Errs = [SqlError]

handlers : All (Handler ()) Errs
handlers = [ printLn ]

--------------------------------------------------------------------------------
--          Records
--------------------------------------------------------------------------------

record Molecule a where
  constructor M
  id        : Bits32
  name      : String
  casNr     : Maybe String
  molWeight : Maybe Double
  type      : a

%runElab derive "Molecule" [Show, Eq, AsRow]

record File where
  constructor F
  id        : Bits32
  content   : ByteString

%runElab derive "File" [Show, Eq, AsRow]

--------------------------------------------------------------------------------
--          App
--------------------------------------------------------------------------------

Molecules : Table
Molecules =
  T "molecules"
    [ C "id"        INTEGER
    , C "name"      TEXT
    , C "casnr"     TEXT
    , C "molweight" REAL
    , C "type"      TEXT
    ]

Files : Table
Files =
  T "files"
    [ C "id"        INTEGER
    , C "content"   BLOB
    ]

insertMol : String -> Maybe String -> Maybe Double -> MolType -> Cmd TInsert
insertMol n c m t = insert Molecules ["name", "casnr", "molweight", "type"] [n,c,m,t]

insertFile : ByteString -> Cmd TInsert
insertFile ix = insert Files ["content"] [ix]

mol : Query [INTEGER, TEXT, TEXT, REAL, TEXT]
mol = SELECT_FROM Molecules ["id", "name", "casnr", "molweight", "type"] ("id" > 1)

fil : Query [INTEGER,BLOB]
fil = SELECT_FROM Files ["id","content"] TRUE

app : App Errs ()
app = withDB ":memory:" $ do
  cmds
    [ if_not_exists $
        createTable Molecules
          [PrimaryKey ["id"], AutoIncrement "id", Unique ["casnr"]]
    , if_not_exists $
        createTable Files
          [PrimaryKey ["id"], AutoIncrement "id"]
    , insertMol "Ethanol"    (Just "64-17-5") (Just 46.069) Compound
    , insertMol "Strychnine" (Just "57-24-9") (Just 334.419) Compound
    , insertMol "Atropine"   (Just "51-55-8") (Just 289.375) Compound
    , insertMol "Sub1"       Nothing          Nothing Polymer
    , insertFile (pack [0x01, 0x02, 0xab, 0xcf, 0xff, 0x1a])
    , insertFile (pack [250..255])
    ]
  ms <- query {a = Molecule MolType} mol 1000
  traverse_ printLn ms
  fs <- query {a = File} fil 10
  traverse_ (putStrLn . encodeBytes . content) fs

main : IO ()
main = runApp handlers app
