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
    [ IF_NOT_EXISTS createMolecules
    , IF_NOT_EXISTS createFiles
    , IF_NOT_EXISTS createUnits
    , IF_NOT_EXISTS createEmployees
    , IF_NOT_EXISTS createEdges
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
    , insertEdge "A" "B"
    , insertEdge "B" "C"
    , insertEdge "B" "D"
    , insertEdge "C" "E"
    , insertEdge "C" "F"
    , insertEdge "D" "F"
    , insertEdge "F" "G"
    , insertEdge "B" "F"
    ] ++ fromList (insertFile . file <$> [0..255])

  queryTable (mol TRUE) 1000 >>= printTable
  putStrLn ""

  queryTable (file TRUE `LIMIT` 20) 1000 >>= printTable
  putStrLn ""

  queryTable employee 1000 >>= printTable
  putStrLn ""

  queryTable heads 1000 >>= printTable
  putStrLn ""

  queryTable tuples 1000 >>= printTable
  putStrLn ""

  queryTable nonHeads 1000 >>= printTable
  putStrLn ""

  queryTable unitStats 1000 >>= printTable
  putStrLn ""

  queryTable parents 1000 >>= printTable

main : IO ()
main = runApp handlers app
