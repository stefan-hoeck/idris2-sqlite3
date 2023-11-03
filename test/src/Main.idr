module Main

import Control.RIO.Sqlite3
import Data.Buffer.Indexed
import Data.ByteString
import Data.List.Quantifiers
import Derive.Prelude

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

record Molecule where
  constructor M
  id        : Bits32
  name      : String
  casNr     : Maybe String
  molWeight : Maybe Double

%runElab derive "Molecule" [Show, Eq]

AsRow Molecule [INTEGER,TEXT,TEXT,REAL] where
  toRow (M i n c m) = [toCell i, toCell n, toCell c, toCell m]
  fromRow [i,n,c,m] = [| M (fromCell i) (fromCell n) (fromCell c) (fromCell m) |]

record File where
  constructor F
  id        : Bits32
  content   : ByteString

%runElab derive "File" [Show, Eq]

AsRow File [INTEGER,BLOB] where
  toRow (F i c) = [toCell i, toCell c]
  fromRow [i,c] = [| F (fromCell i) (fromCell c) |]

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
    ]

Files : Table
Files =
  T "files"
    [ C "id"        INTEGER
    , C "content"   BLOB
    ]

insertMol : String -> Maybe String -> Maybe Double -> Cmd TInsert
insertMol n c m = insert Molecules ["name", "casnr", "molweight"] [n,c,m]

insertFile : ByteString -> Cmd TInsert
insertFile ix = insert Files ["content"] [ix]

mol : Query [INTEGER, TEXT, TEXT, REAL]
mol = SELECT_FROM Molecules ["id", "name", "casnr", "molweight"] ("id" > 1)

fil : Query [INTEGER,BLOB]
fil = SELECT_FROM Files ["id","content"] ("id" == 1)

app : App Errs ()
app = withDB ":memory:" $ do
  cmds
    [ if_not_exists $
        createTable Molecules
          [PrimaryKey ["id"], AutoIncrement "id", Unique ["casnr"]]
    , if_not_exists $
        createTable Files
          [PrimaryKey ["id"], AutoIncrement "id"]
    , insertMol "Ethanol"    (Just "64-17-5") (Just 46.069)
    , insertMol "Strychnine" (Just "57-24-9") (Just 334.419)
    , insertMol "Atropine"   (Just "51-55-8") (Just 289.375)
    , insertMol "Sub1"       Nothing          Nothing
    , insertFile (pack [0x01, 0x02, 0xab, 0xcf, 0xff, 0x1a])
    ]
  ms <- query {a = Molecule} mol 1000
  traverse_ printLn ms
  fs <- query {a = File} fil 1
  traverse_ (putStrLn . encodeBytes . content) fs

main : IO ()
main = runApp handlers app
