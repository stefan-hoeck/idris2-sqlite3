module Main

import Control.RIO.Sqlite3
import Data.Buffer.Indexed
import Data.ByteString
import Data.List.Quantifiers
import Data.WithID
import Enum
import Schema

%default total

--------------------------------------------------------------------------------
--          Errors
--------------------------------------------------------------------------------

0 Errs : List Type
Errs = [SqlError]

handlers : All (Handler ()) Errs
handlers = [ printLn ]

file : Bits8 -> File
file x = F $ pack [x,x+1,x+2]

--------------------------------------------------------------------------------
--          App
--------------------------------------------------------------------------------

app : App Errs ()
app = withDB ":memory:" $ do
  cmds $
    [ if_not_exists createMolecules
    , if_not_exists createFiles
    , if_not_exists createUnits
    , if_not_exists createEmployees
    , insertMol $ M "Ethanol"    (Just "64-17-5") (Just 46.069) Compound
    , insertMol $ M "Strychnine" (Just "57-24-9") (Just 334.419) Compound
    , insertMol $ M "Atropine"   (Just "51-55-8") (Just 289.375) Compound
    , insertMol $ M "Sub1"       Nothing          Nothing Polymer
    , insertUnit $ U "Sales" 1
    , insertUnit $ U "R&D" 2
    , insertUnit $ U "HR" 3
    , insertEmployee $ E "Sarah" 8300.0 1
    , insertEmployee $ E "Ben" 8000.0 2
    , insertEmployee $ E "Gil" 7750.0 3
    , insertEmployee $ E "Cathy" 3000.0 1
    , insertEmployee $ E "John" 3100.0 1
    , insertEmployee $ E "Abby" 3000.0 2
    , insertEmployee $ E "May" 3100.0 2
    , insertEmployee $ E "Brian" 3000.0 2
    , insertEmployee $ E "Benny" 3100.0 2
    , insertEmployee $ E "Rob" 3100.0 3
    , insertEmployee $ E "Zelda" 3100.0 3
    , insertEmployee $ E "Gundi" 2050.0 1
    , insertEmployee $ E "Valeri" 5010.0 1
    , insertEmployee $ E "Ronja" 4010.0 1
    ] ++ fromList (insertFile . file <$> [0..255])
  ms <- query (mol TRUE) 1000
  traverse_ printLn ms
  fs <- query (file TRUE `LIMIT` 20) 1000
  traverse_ (putStrLn . encodeBytes . content . value) fs
  es <- query employee 1000
  traverse_ printLn es
  hs <- query heads 1000
  traverse_ printLn hs
  ts <- query tuples 1000
  traverse_ printLn ts
  ps <- query nonHeads 1000
  traverse_ printLn ps
  ss <- query unitStats 1000
  traverse_ printLn ss

main : IO ()
main = runApp handlers app
