module FS.Sqlite3

import public Sqlite3
import public FS

%default total

export
Resource (Async e) Stmt where
  cleanup = liftIO . sqliteFinalize'

export
Resource (Async e) DB where
  cleanup = liftIO . sqliteClose'

parameters {auto has : Has SqlError es}

  export %inline
  openSqlite : String -> Async e es DB
  openSqlite = injectIO . sqliteOpen

  export %inline
  openStmt : DB => String -> Async e es Stmt
  openStmt = injectIO . sqlitePrepare

  ||| Open a connection to the given database and use it to
  ||| run the given effectful computation.
  |||
  ||| This comes with the guarantees that the connection is properly
  ||| closed at the end.
  export
  withDB : String -> (DB => Async e es a) -> Async e es a
  withDB s f = use1 (openSqlite s) $ \_ => f

  ||| Prepare an SQL statement and use it to run the given effectful computation.
  |||
  ||| This comes with the guarantees that the statement is properly
  ||| finalized at the end.
  export
  withStmt : DB => String -> (Stmt => Async e es a) -> Async e es a
  withStmt s f = use1 (openStmt s) $ \_ => f

  ||| Prepare an SQL statement and use it to run the given effectful computation.
  |||
  ||| This comes with the guarantees that the statement is properly
  ||| finalized at the end.
  export
  bindParams : DB => Stmt => List Parameter -> Async e es ()
  bindParams = injectIO . sqliteBind

  export
  openBoundStmt : DB => ParamStmt -> Async e es Stmt
  openBoundStmt st = Prelude.do
    let (ps, str) := runState init st
    st <- openStmt str
    bindParams ps.args
    pure st

  ||| Prepare an SQL statement and use it to run the given effectful computation.
  |||
  ||| This comes with the guarantees that the statement is properly
  ||| finalized at the end.
  |||
  ||| This works just like `withStmt` but it also bind the given arguments.
  export
  withBoundStmt : DB => ParamStmt -> (Stmt => Async e es a) -> Async e es a
  withBoundStmt st f = use1 (openBoundStmt st) $ \_ => f

  ||| Runs an SQL statement, returning the response from the database.
  export
  step : (s : Stmt) => Async e es SqlResult
  step @{s} = liftIO $ sqliteStep s

  ||| Prepares, executes and finalizes the given SQL statement.
  |||
  ||| The statement may hold a list of parameters, which will be
  ||| bound prior to executing the statement.
  export
  commit : DB => ParamStmt -> Async e es ()
  commit st = withBoundStmt st (ignore step)

  ||| Prepares and executes the given SQL query and extracts up to
  ||| `n` rows of results.
  export
  selectRows : DB => FromRow a => ParamStmt -> (n : Nat) -> Async e es (List a)
  selectRows st n = withBoundStmt st (injectIO $ loadRows n)

  ||| Prepares and executes the given SQL query and extracts the
  ||| first result.
  export
  selectRow : DB => FromRow a => ParamStmt -> Async e es a
  selectRow st = do
    [v] <- selectRows st 1 | _ => throw NoMoreData
    pure v

  ||| Prepares and executes the given SQL query and extracts the
  ||| first result (if any).
  export
  findRow : DB => FromRow a => ParamStmt -> Async e es (Maybe a)
  findRow st = do
    [v] <- selectRows st 1 | _ => pure Nothing
    pure $ Just v

  export
  rows :
       {auto cs : ChunkSize}
    -> {auto db : DB}
    -> {auto fr : FromRow a}
    -> ParamStmt
    -> AsyncStream e es (List a)
  rows {cs = CS sz} st =
    resource (openBoundStmt st) $ \_ =>
      unfoldEvalMaybe $
        (\case [] => Nothing; xs => Just xs) <$> injectIO (loadRows sz)

--------------------------------------------------------------------------------
-- Runnings Commands
--------------------------------------------------------------------------------

  ||| Executes the given SQL command.
  export %inline
  cmd : DB => Cmd t -> Async e es ()
  cmd = commit . encodeCmd

  rollback : DB => HSum es -> Async e es a
  rollback x = ignore (withStmt "ROLLBACK TRANSACTION" step) >> fail x

  ||| Runs several commands in a single transaction.
  |||
  ||| If any of the commands fails, the whole transaction is rolled back.
  export %inline
  cmds : DB => Cmds -> Async e es ()
  cmds cs =
    uncancelable $ \poll => Prelude.do
      ignore $ withStmt "BEGIN TRANSACTION" step
      handleErrors rollback (runCommands cs)
      ignore $ withStmt "COMMIT TRANSACTION" step

    where
      runCommands : Cmds -> Async e es ()
      runCommands []      = pure ()
      runCommands (c::cs) = cmd c >> runCommands cs

  ||| Runs the given query and accumulates at most `n` rows.
  export %inline
  query : DB => Query t -> (n : Nat) -> Async e es (List t)
  query q = selectRows (encodeQuery q)

  ||| Runs the given query and returns the first result (if any).
  export %inline
  query1 : DB => Query t -> Async e es (Maybe t)
  query1 q = map (\case h::_ => Just h; [] => Nothing) $ query q 1

  ||| Streams the rows resulting from running the given query.
  export %inline
  queryRows : (cs : ChunkSize) => DB => Query t -> AsyncStream e es (List t)
  queryRows q = rows (encodeQuery q)

  ||| Runs the given query and accumulates at most `n` rows.
  |||
  ||| The result is stored in a `Table` with a proper header of
  ||| column names.
  export
  queryTable :
       {auto db : DB}
    -> {auto tr : ToRow t}
    -> (q : Query t)
    -> {auto 0 prf : ToRowTypes t === FromRowTypes t}
    -> Nat
    -> Async e es (Table t)
  queryTable {prf} q n = do
    rs <- query q n
    pure (T (rewrite prf in hmap columnName q.columns) rs)
