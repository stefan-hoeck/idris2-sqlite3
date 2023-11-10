# Introduction to idris2-sqlite3

In this tutorial we are going to set up a small in-memory
database and demonstrate how to add and remove entries from it as well
as running typical queries and converting rows from and to
Idris types. We start with the necessary imports:

```idris
module Tutorial

import Data.WithID
import Derive.Sqlite3
import Control.RIO.Sqlite3

%default total
%language ElabReflection
```

## Tables and Schemata

### SQLite Types

Before we start defining the tables for our database, we need to quickly
talk about the column types supported by SQLite. Although SQLite is dynamically
typed, and columns can hold values of different types, we currently treat
tables and columns to be monomorphic (all column entries have the same
type) in this library. There are five types supported by SQLite, which
is reflected in the `Sqlite3.Types.SqliteType` enumeration: `INTEGER`
for signed 64 bit integers, `REAL` for 64 bit floating point numbers,
`TEXT`for strings of unicode characters, and `BLOB` for byte arrays.

Data type `SqliteType` comes with a fifth value called `BOOL` for boolean
values. In database tables, this is represented as an integer with `0`
corresponding to `False` and every other value (typically `1`) corresponding
to `True`. Although this is not an official SQLite type, it helps with
defining and typing predicate expressions as we will later see.

Each of the five values of `SqliteType` corresponds to an Idris type,
to which values are converted when reading from and writing to the database.
This is reflected in function `Sqlite3.Types.IdrisType` and summarized
in the following table:

| SQLite Type | Idris Type | Format in database             |
|-------------|------------|--------------------------------|
| INTEGER     | Int64      | 64 bit signed integer          |
| REAL        | Double     | 64 bit floating point number   |
| TEXT        | String     | sequence of unicode characters |
| BLOB        | ByteString | byte array                     |
| BOOL        | Bool       | 64 bit signed integer          |

All SQLite types are nullable, that is, `NULL` is a valid value in a column,
unless the `NOT NULL` constraint has been set (see below). Therefore,
when reading a single table cell via the C-API, we always return a
`Maybe`, where `Nothing` represents a `NULL` entry in the table.

### Defining Tables

We are now ready to define our first table. Our database should
allow us to store students and exams and compute marks depending
on the number of points a student got in each exercise of an
exam.

First, let's define the table for students. We keep this very
basic:

```idris
Students : Table
Students =
  table "students"
    [ C "student_id" INTEGER
    , C "name"       TEXT
    , C "email"      TEXT
    ]
```

As you can see, a `Table` consists of a name and a list of columns,
where each column again has a name and an associated SQLite type.

Actually, a table is a record of three field, where the third field
called `as` allows us to rename tables during queries. It is therefore
advisable to use the `table` utility constructor when defining new
tables.

Here are the other tables we are going to use:

```idris
Problems : Table
Problems =
  table "problems"
    [ C "problem_id"   INTEGER
    , C "points"       REAL
    , C "problem_text" TEXT
    ]

Exams : Table
Exams =
  table "exams"
    [ C "exam_id"      INTEGER
    , C "topic"        TEXT
    , C "term"         TEXT
    , C "year"         INTEGER
    ]

ExamProblems : Table
ExamProblems =
  table "exam_problems"
    [ C "problem_id"   INTEGER
    , C "exam_id"      INTEGER
    ]

StudentProblems : Table
StudentProblems =
  table "student_problems"
    [ C "problem_id"   INTEGER
    , C "student_id"   INTEGER
    , C "points"       REAL
    ]
```

So, we have a table for problems the students have to solve during
exams, as well as a table of exams listing the exam's topic, as well
as the term and year when the exam was held. In addition, we define two
link tables for the many-to-many relations between problems and exams,
as well as problems and students who solved them.

<!-- vi: filetype=idris2:syntax=markdown
-->
