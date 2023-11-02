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

  ||| Prepare an SQL statement and use it to run the given effectful computation.
  |||
  ||| This comes with the guarantees that the statement is properly
  ||| finalized at the end.
  |||
  ||| This works just like `withStmt` but it also bind the given arguments.
  export
  bindParams : DB => Stmt => List Parameter -> App es ()
  bindParams ps = injectIO (sqliteBind ps)

  ||| Prepare an SQL statement and use it to run the given effectful computation.
  |||
  ||| This comes with the guarantees that the statement is properly
  ||| finalized at the end.
  |||
  ||| This works just like `withStmt` but it also bind the given arguments.
  export
  withBoundStmt : DB => String -> List Parameter -> (Stmt => App es a) -> App es a
  withBoundStmt str ps f = withStmt str (bindParams ps >> f)

  ||| Runs an SQL statement, returning the response from the database.
  export
  step : (s : Stmt) => App es SqlResult
  step @{s} = liftIO $ sqliteStep s

  ||| Prepares, executes and finalizes the given SQL statement.
  |||
  ||| The statement may hold a list of parameters, which will be
  ||| bound prior to executing the statement.
  export
  commit : DB => String -> List Parameter -> App es ()
  commit str ps = withBoundStmt str ps (ignore step)

  ||| Prepares and executes the given SQL query and extracts up to
  ||| `n` rows of results.
  export
  selectRows : DB => String -> List Parameter -> (n : Nat) -> App es (List Row)
  selectRows str ps n = withBoundStmt str ps (injectIO $ loadRows n)

  ||| Prepares and executes the given SQL query and extracts the
  ||| first result.
  export
  selectRow : DB => String -> List Parameter -> App es Row
  selectRow str ps = do
    [v] <- selectRows str ps 1 | _ => throw NoMoreData
    pure v

  ||| Prepares and executes the given SQL query and extracts the
  ||| first result (if any).
  export
  findRow : DB => String -> List Parameter -> App es (Maybe Row)
  findRow str ps = do
    [v] <- selectRows str ps 1 | _ => pure Nothing
    pure $ Just v
