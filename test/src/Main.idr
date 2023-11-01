module Main

import Data.Buffer.Indexed
import Data.ByteString
import Control.RIO.Sqlite3

%default total

--------------------------------------------------------------------------------
--          Errors
--------------------------------------------------------------------------------

0 Errs : List Type
Errs = [SqlError]

handlers : All (Handler ()) Errs
handlers = [ printLn ]

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

mol : Query [Bits32, String, Maybe String, Maybe Double]
mol =
  SELECT_FROM Molecules [C "id", C "name", C "casnr", C "molweight"] (C "id" > 1)

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
  ms <- query mol 1000
  traverse_ printLn ms

main : IO ()
main = runApp handlers app
