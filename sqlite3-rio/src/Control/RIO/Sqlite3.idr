module Control.RIO.Sqlite3

import public Control.RIO.App
import public Sqlite3

%default total

parameters {auto has : Has SqlError es}

  ||| Open a connection to the given database and use it to
  ||| run the given effectful computation.
  |||
  ||| This comes with the guarantees that the connection is properly
  ||| closed at the end.
  export
  withDB : String -> (DB => App es a) -> App es a
  withDB s f = do
    db <- injectIO $ sqliteOpen s
    finally (liftIO $ sqliteClose' db) (f @{db})

  ||| Prepare an SQL statement and use it to run the given effectful computation.
  |||
  ||| This comes with the guarantees that the statement is properly
  ||| finalized at the end.
  export
  withStmt : DB => String -> (Stmt => App es a) -> App es a
  withStmt str f = do
    stmt <- injectIO $ sqlitePrepare str
    finally (liftIO $ sqliteFinalize' stmt) (f @{stmt})

  ||| Runs an SQL statement, returning the response from the database.
  export
  step : (s : Stmt) => App es SqlResult
  step @{s} = liftIO $ sqliteStep s

  ||| Prepares, executes and finalizes the given SQL statement.
  export
  commit : DB => String -> App es ()
  commit str = withStmt str (ignore step)

  ||| Prepares and executes the given SQL query and extracts up to
  ||| `n` rows of results.
  export
  select : DB => {ts : _} -> String -> (n : Nat) -> App es (Table ts)
  select str n = withStmt str (injectIO $ loadRows n)

  ||| Prepares and executes the given SQL query and extracts the
  ||| first result.
  export
  select1 : DB => {ts : _} -> String -> App es (Row ts)
  select1 str = do
    [v] <- select str 1 | _ => throw NoMoreData
    pure v

  ||| Prepares and executes the given SQL query and extracts the
  ||| first result (if any).
  export
  selectMaybe : DB => {ts : _} -> String -> App es (Maybe $ Row ts)
  selectMaybe str = do
    [v] <- select str 1 | _ => pure Nothing
    pure $ Just v
