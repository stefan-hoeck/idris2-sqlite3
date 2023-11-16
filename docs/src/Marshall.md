# Marshalling Idris Types from and to SQLite

```idris
module Marshall

import Control.RIO.Sqlite3
import Data.String
import Derive.Sqlite3
import Tutorial

%default total
%language ElabReflection

data Id : (v : k) -> Type where
  I : Bits32 -> Id k

%runElab deriveIndexed "Id" [Show,Eq,Ord,Num,FromCell,ToCell]

data Kind = Stu | Ex | Pro

record Student i where
  constructor MkStudent
  student_id : i
  name       : String
  email      : Email

%runElab derive "Student" [Show,Eq,ToRow,FromRow]

record Exam i where
  constructor MkExam
  exam_id : i
  topic   : Topic
  term    : Term
  year    : Bits16

%runElab derive "Exam" [Show,Eq,ToRow,FromRow]

record Problem i where
  constructor MkProblem
  problem_id : i
  points     : Double
  text       : String

%runElab derive "Problem" [Show,Eq,ToRow,FromRow]

record Summary where
  constructor MkSummary
  exam    : Exam (Id Ex)
  student : Student (Id Stu)
  points  : Double
  tot     : Double

%runElab derive "Summary" [Show,Eq,ToRow,FromRow]

totPoints : Query Summary
totPoints =
  SELECT
    [ "e.exam_id"
    , "e.topic"
    , "e.term"
    , "e.year"
    , "s.student_id"
    , "s.name"
    , "s.email"
    , SUM "sp.points" `AS` "ex_points"
    , SUM "p.points"  `AS` "total"
    ]
    [< FROM (ExamProblems   `AS` "ep")
    ,  JOIN (Exams          `AS` "e")  `USING` ["exam_id"]
    ,  JOIN (Problems       `AS` "p")  `USING` ["problem_id"]
    ,  CROSS_JOIN (Students `AS` "s")
    ,  OUTER_JOIN (StudentProblems `AS` "sp") `USING` ["problem_id", "student_id"]
    ]
  `GROUP_BY` ["e.exam_id","s.student_id"]
  `HAVING`   (MIN (COALESCE ["sp.points", (-1)]) >= 0)
  `ORDER_BY` [ASC "e.exam_id", ASC "ex_points"]

app : App [SqlError] ()
app =
  withDB ":memory:" $ do
    putStrLn "Populating the database"
    populateDB

    putStrLn "\nPoints per student and exam:"
    queryTable totPoints 1000 >>= printTable

main : IO ()
main = runApp [printLn] app
```

<!-- vi: filetype=idris2:syntax=markdown
-->
