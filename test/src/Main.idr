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

insert : DB => Int64 -> String -> String -> Double -> App Errs ()
insert ix nm cas w =
  commit
    "INSERT INTO Molecules VALUES(:1,:2,:3,:4);"
    [ P ":1" INTEGER ix
    , P ":2" TEXT nm
    , P ":3" TEXT cas
    , P ":4" REAL w
    ]

insertBlob : DB => Int64 -> ByteString -> App Errs ()
insertBlob ix bs =
  commit
    "INSERT INTO Files VALUES(:1,:2);"
    [ P ":1" INTEGER ix
    , P ":2" BLOB bs
    ]

app : App Errs ()
app = withDB ":memory:" $ do
  commit "CREATE TABLE Molecules(Id INTEGER, Name TEXT, CasNr Text, MolWeight REAL);" []
  insert 1 "Ethanol" "64-17-5" 46.069
  insert 2 "Strychnine" "57-24-9" 334.419
  insert 3 "Atropine" "51-55-8" 289.375
  vs <- selectRows "SELECT Name,MolWeight FROM Molecules" [] 10
  traverse_ printLn vs
  v <- selectRow "SELECT Name,MolWeight FROM Molecules" []
  printLn v
  v <- findRow "SELECT Name,MolWeight FROM Molecules" []
  printLn v

  commit "CREATE TABLE Files(Id INTEGER, Content BLOB);" []
  insertBlob 1 (pack [0x01, 0x02, 0xab, 0xcf, 0xff, 0x1a])
  v <- selectRow "SELECT Id,Content FROM Files" []
  printLn v

main : IO ()
main = runApp handlers app
