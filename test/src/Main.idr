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

app : App Errs ()
app = withDB ":memory:" $ do
  commit "CREATE TABLE Molecules(Id INTEGER, Name TEXT, CasNr Text, MolWeight REAL);"
  commit "INSERT INTO Molecules VALUES(1, 'Ethanol', '64-17-5', 46.069);"
  commit "INSERT INTO Molecules VALUES(2, 'Strychnine', '57-24-9', 334.419);"
  commit "INSERT INTO Molecules VALUES(2, 'Atropine', '51-55-8', 289.375);"
  vs <- select {ts = [TEXT,REAL]} "SELECT Name,MolWeight FROM Molecules" 10
  traverse_ printLn vs
  v <- select1 {ts = [TEXT,REAL]} "SELECT Name,MolWeight FROM Molecules"
  printLn v
  v <- selectMaybe {ts = [TEXT,REAL]} "SELECT Name,MolWeight FROM Molecules"
  printLn v

main : IO ()
main = runApp handlers app
