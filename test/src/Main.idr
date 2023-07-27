module Main

import Data.Buffer.Indexed
import Data.ByteString
import Control.RIO.App
import Sqlite3

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
--          RIO Bindings
--------------------------------------------------------------------------------

withDB : Has SqlError es => String -> (DB => App es a) -> App es a
withDB s f = do
  db <- injectIO $ sqliteOpen s
  finally (liftIO $ sqliteClose' db) (f @{db})

withStmt : Has SqlError es => DB => String -> (Stmt => App es a) -> App es a
withStmt str f = do
  stmt <- injectIO $ sqlitePrepare str
  finally (liftIO $ sqliteFinalize' stmt) (f @{stmt})

export
step : Has SqlError es => (s : Stmt) => App es SqlResult
step = liftIO $ sqliteStep s

export
commit : Has SqlError es => DB => String -> App es ()
commit str = withStmt str (ignore step)

export
select : Has SqlError es => DB => {ts : _} -> String -> App es (Table ts)
select str = withStmt str (injectIO $ loadRows 1000000)

--------------------------------------------------------------------------------
--          App
--------------------------------------------------------------------------------

app : App Errs ()
app = withDB ":memory:" $ do
  commit "CREATE TABLE Molecules(Id INTEGER, Name TEXT, CasNr Text, MolWeight REAL);"
  commit "INSERT INTO Molecules VALUES(1, 'Ethanol', '64-17-5', 46.069);"
  commit "INSERT INTO Molecules VALUES(2, 'Strychnine', '57-24-9', 334.419);"
  commit "INSERT INTO Molecules VALUES(2, 'Atropine', '51-55-8', 289.375);"
  vs <- select {ts = [TEXT,REAL]} "SELECT Name,MolWeight FROM Molecules"
  traverse_ printLn vs

main : IO ()
main = runApp handlers app
