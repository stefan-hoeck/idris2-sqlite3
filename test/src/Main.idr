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
handlers =
  [ printLn
  ]

--------------------------------------------------------------------------------
--          App
--------------------------------------------------------------------------------

insert : DB => Int64 -> String -> String -> Double -> App Errs ()
insert ix nm cas w =
  commit_
    "INSERT INTO Molecules VALUES(:v,:w,:x,:y);"
    [ A ":v" INTEGER ix
    , A ":w" TEXT (Just nm)
    , A ":x" TEXT (Just cas)
    , A ":y" REAL w
    ]

insertBlob : DB => Int64 -> ByteString -> App Errs ()
insertBlob ix bs =
  commit_
    "INSERT INTO Files VALUES(:v,:w);"
    [ A ":v" INTEGER ix
    , A ":w" BLOB (Just bs)
    ]

app : App Errs ()
app = withDB ":memory:" $ do
  commit "CREATE TABLE Molecules(Id INTEGER, Name TEXT, CasNr Text, MolWeight REAL);"
  insert 1 "Ethanol" "64-17-5" 46.069
  insert 2 "Strychnine" "57-24-9" 334.419
  insert 3 "Atropine" "51-55-8" 289.375
  vs <- select {ts = [TEXT,REAL]} "SELECT Name,MolWeight FROM Molecules" 10
  traverse_ printLn vs
  v <- select1 {ts = [TEXT,REAL]} "SELECT Name,MolWeight FROM Molecules"
  printLn v
  v <- selectMaybe {ts = [TEXT,REAL]} "SELECT Name,MolWeight FROM Molecules"
  printLn v

  commit "CREATE TABLE Files(Id INTEGER, Content BLOB);"
  insertBlob 1 (pack [1,2,3])
  v <- selectMaybe {ts = [INTEGER,BLOB]} "SELECT Id,Content FROM Files"
  printLn v

main : IO ()
main = runApp handlers app
