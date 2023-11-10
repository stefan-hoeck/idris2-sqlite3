# Introduction to idris2-sqlite3

In this tutorial, we are going to set up a small in-memory
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
typed and columns can hold values of different types, we currently treat
tables and columns to be monomorphic (all column entries have the same
type) in this library. There are four types supported by SQLite, which
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

### Creating Tables

As you might have noted, our table definitions do not list any
table or column constraints. Those are only given when we create
new tables. In order to do so, we need to assemble the corresponding
`Sqlite3.Cmd.Cmd`: This type provides a DSL (domain specific language)
for safely generating the SQL statements necessary to for instance
create tables and insert, update, or delete rows.

Here's the command for creating the `students` table:

```idris
createStudents : Cmd TCreate
createStudents =
  IF_NOT_EXISTS $ CREATE_TABLE Students
    [ PRIMARY_KEY   ["student_id"]
    , AUTOINCREMENT "student_id"
    , NOT_NULL      "name"
    , NOT_NULL      "email"
    ]
```

Let's break this down a bit: `CREATE_TABLE` is a utility constructor
for the `CreateTable` data constructor of `Cmd`. `Cmd` itself is indexed
with a `CmdType` to facilitate defining utility functions such as
`IF_NOT_EXISTS`, which work only on a subset of commands.

In general, if you see an all-caps data constructor or function
name, this typically corresponds directly to an SQL command or keyword.
In case a command consists of more than one keyword (such as `IF NOT EXISTS`
in SQL), spaces are replaced by underscores in the Idris identifier.
All-caps identifiers always correspond to the recommended way of using
a data constructor or function, so that the commands and expressions
we write in Idris typically resemble the corresponding SQL expressions
quite closely.

Besides a `Table` argument, `CREATE_TABLE` takes a list of
table constraints of type `Sqlite3.Cmd.Constraint`, which is indexed by
the table for which we define the constraints. And this is where the
interesting stuff begins: Whenever we use a string literal to identify
a column in a table or list of tables, Idris verifies for us that
the column is named correctly. For instance, the following fails with
a (not very helpful) error, because the last `NOT_NULL` constraint
has a typo in the column name:

```idris
failing "Can't find an implementation for IsJust"
  createStudentsFailing : Cmd TCreate
  createStudentsFailing =
    IF_NOT_EXISTS $ CREATE_TABLE Students
      [ PRIMARY_KEY   ["student_id"]
      , AUTOINCREMENT "student_id"
      , NOT_NULL      "name"
      , NOT_NULL      "umail"
      ]
```

I quickly implement the other commands for creating our tables,
and the we'll have a closer look at how table and column names
are resolved at compile time.

```idris
createExams : Cmd TCreate
createExams =
  IF_NOT_EXISTS $ CREATE_TABLE Exams
    [ PRIMARY_KEY   ["exam_id"]
    , AUTOINCREMENT "exam_id"
    , NOT_NULL      "topic"
    , NOT_NULL      "term"
    , NOT_NULL      "year"
    ]

createProblems : Cmd TCreate
createProblems =
  IF_NOT_EXISTS $ CREATE_TABLE Problems
    [ PRIMARY_KEY   ["problem_id"]
    , AUTOINCREMENT "problem_id"
    , NOT_NULL      "problem_text"
    , NOT_NULL      "points"
    , CHECK         ("points" > 0)
    ]

createExamProblems : Cmd TCreate
createExamProblems =
  IF_NOT_EXISTS $ CREATE_TABLE ExamProblems
    [ PRIMARY_KEY ["problem_id", "exam_id"]
    , FOREIGN_KEY Problems ["problem_id"] ["problem_id"]
    , FOREIGN_KEY Exams ["exam_id"] ["exam_id"]
    ]

createStudentProblems : Cmd TCreate
createStudentProblems =
  IF_NOT_EXISTS $ CREATE_TABLE StudentProblems
    [ PRIMARY_KEY   ["problem_id", "student_id"]
    , FOREIGN_KEY Problems ["problem_id"] ["problem_id"]
    , FOREIGN_KEY Students ["student_id"] ["student_id"]
    , NOT_NULL "points"
    ]
```

Three new things showed up in these additional table definitions:
First, primary keys can consist of more than one column, that's
why we provide them as a list of columns (actually, its a
heterogeneous list of type `All (TColumn t) ts`, where `t` is
the table we are currently working on, and `ts` is the list of
`SqliteType`s associated with the columns). Second, the
last two examples show how we can define `FOREIGN KEY` constraints,
and third, in `CHECK` we used an expression of type `Sqlite3.Expr.Expr`
for the first time. We will look at expressions in more detail
below, but I can already reveal that expressions are properly
typed and checked by Idris.

## Behind the Scenes: Find Columns and their Types via String Literals

What we saw so far, was very basic Idris programming: We just used
a bunch of data constructors or utilities thereof to define Idris
values representing SQL statements and commands. I'll still have to
show, how to use these to actually run the commands they represent
against a real SQLite table. But before I do that, we are going
to have a detailed look at what's going on in the background: How
does Idris resolve column names and figure out the types associated
with columns. I have looked at several libraries in other functional
languages providing DSLs for interacting with the world of relational
databases, and while many of them came with nice introductory tutorials,
none of them explained in detail how everything and why these libraries
are implemented.

At least in our case, the implementation is not very hard to
understand, so here we go.

### Looking up Table Columns

In module `Sqlite3.Table`, there is a very simple
function called `FindCol`, which tries to lookup a column in a
list of column and return its type. Since this might fail,
it wraps the result in a `Maybe`. But then there is a second
function called `ListColType`, which does the same thing by invoking
`FindCol` but it takes an additional proof that `FindCol`
returns a `Just` for the current column name and list of columns.

This is the core design decision behind name resolution in this
library: Instead of using an inductive type such as `Elem` for lists
for proofing that a value is present in a container, we define
a lookup function that returns a `Nothing` in case the lookup fails.
This decision has two consequences, one good, the other rather
restricting: First, name resolutions mainly occurs by unification
instead of by proof search, that is, Idris will evaluate the
result of invoking `FindCol` with the current arguments, and
it will then check, whether the result is a `Just`. This is
both faster than inductive proof search, and it is not restricted
by the default proof search limit: We can lookup names in very
large lists of columns.

For instance:

```idris
Tbl : Table
Tbl =
  table "foo" $
    map (\n => C ("x" ++ prim__cast_Bits8String n) INTEGER) [0..55]

test : Expr [<Tbl] INTEGER
test = "x55"
```

Function `test` typechecks in a reasonable amount of time, while
it would fail with an exception in case of an inductive
proof search approach unless we would increase the proof search depth
limit before its implementation.

However, this decision can also be rather restrictive: All values
and table definitions involved must be known at compile time. It is
therefore assumed that the database schema has been declared and
publicly exported as in the examples above. On the other hand it
is hard (or even impossible without resorting to `believe_me`) to
come up with these proofs in the presence of abstract table
definitions. Only time will tell if this will pay out in the end,
but this is what we currently have.

### Wrapping up Column Names with their Types

In addition to `ListColType`, there is also function
`TableColType`, which does exactly the same thing but for
the columns wrapped up in a value of type `Table`. The
interesting thing is, that we can now define a data type
for typed column names (called `Sqlite3.Table.TColumn`),
that represents a column in a table indexed by the column's
SQLite type. Even better, using the functions defined so far,
this data type comes with a `fromString` function that allows
us to use string literals whenever we want to come up with
a value of type `TColumn`:

```idris
colStudentName : TColumn Students TEXT
colStudentName = "name"
```

This is what we used when using all the string literals
in our table constraints in the `CREATE_TABLE` calls.

But it gets even better: Not only can we use string literals
if the full type of a `TColumn` is known, Idris can also
derive the second index (the SQLite type) of a `TColumn`
just from its name, which is very useful when using string
literals in arbitrary SQL expressions:

```idris
pointsPositive : Expr [<StudentProblems] BOOL
pointsPositive = "points" > 0
```

A lot is going on here: The `(>)` operator is one of the data
constructors of `Sqlite3.Expr.Expr`, which is indexed by
a schema (a snoclist) of tables and the expressions SQLite type.
They types of the expressions on both sides of `(>)` must be identical,
so Idris must have a way of figuring out what to do with the
integer literal on the right. Luckily, the string literal `"points"`
is enough to figure out that the left hand side is of type
`Expr [<StudentProblems] REAL`, because that's what we specified
in the definition of `StudentProblems`. Since the right-hand side
must be of the same type, Idris will now use the implementation
of `Num (Expr s REAL)` to properly convert the integer literal
on the right.

It is paramount that all of the above works smoothly
in order to get nice and concise syntax when defining SQL expressions
and commands. The good new is, that not a lot of complexity
is involved here: Understanding how `fromString` works for
`TColumn` as well as `Expr` - and both are implemented with
the same techniques - makes everything else fall into place.
This is a tremendous improvement over languages such as Haskell,
where a lot of typeclass magic I still don't fully understand is
necessary to achieve similar results.

### Resolving qualified Column Names in a Schema

Once one understands how name resolution behaves for columns
and tables, it is only a small step to understanding name
resolution for column names in schemata. A `Schema` is just
a type alias for `SnocList Table`. We use a `SnocList` here
instead of a `List`, because when defining a query via a
`SELECT` statement, the schema is assembled from left to
right via the `FROM` part and possibly via any named
expressions. We will look at that part in more detail once we
dissect queries.

Name resolution for schemata is implemented in functions `FindSchemaCol`,
which returns a `Maybe` and function `SchemaColType`, which expects
that `FindSchemaCol` returns a `Just` with the current arguments.

The implementation of `FindSchemaCol` is only slightly more involved
that the one of `FindCol`: We first check if the column name is
prefixed with a table name such as in `"studends.name"`, in which
case the table name is looked up followed by looking up the
column name. If the column name is unqualified, such as in `"name"`,
function `FindSchemaCol` will only succeed if either the schema only
consists of a single table, or if the last table in the schema
has an empty string as its name. Here are a couple of examples
to demonstrate this behavior:

```idris
qualifiedName : Expr [<Students,StudentProblems] REAL
qualifiedName = "student_problems.points"

-- we can rename a table in a schema
qualifiedNameWithAlias : Expr [<Students,StudentProblems `AS` "sp"] REAL
qualifiedNameWithAlias = "sp.points"

-- using an unqualified name in a schema with a single table is OK
unqualifiedName1 : Expr [<Students] TEXT
unqualifiedName1 = "email"

-- using an unqualified name in a schema with an unnamed table
-- at the end is OK
unqualifiedName2 : Expr [<Students,T "" "" [C "total" REAL]] REAL
unqualifiedName2 = "total"
```

The last example is required when using named expressions for the
columns collected in a `SELECT` statements: If, for instance, we
want to accumulate the total number of points of each student
in an exam and sort the output according to that total, we can
name the corresponding column and reference it during the
`HAVING` and `ORDER BY` parts of a `SELECT` statement.

Here are a couple of examples that don't work:

```idris
failing "Can't find an implementation for IsJust"
  wrongColName : Expr [<Students,StudentProblems] REAL
  wrongColName = "student_problems.poins"

failing "Can't find an implementation for IsJust"
  wrongTableName : Expr [<Students,StudentProblems] REAL
  wrongTableName = "studet_problems.points"

failing "Can't find an implementation for IsJust"
  usingOldNameOfRenamedTable : Expr [<Students,StudentProblems `AS` "sp"] REAL
  usingOldNameOfRenamedTable = "student_problems.points"

failing "Can't find an implementation for IsJust"
  unqualifiedName : Expr [<Students,StudentProblems `AS` "sp"] REAL
  unqualifiedName = "points"
```

One could argue that the last example (`unqualifiedName`) should
actually be accepted, because it is unambiguous. Currently, we are being
strict here: If you want to refer to a column in named table of
a multi-table schema, you need to give its qualified name. This means
you'll have to type some more, but comes with the benefit that it
is stable and typechecks even when a new column with the same name
is added to another table in the schema.
<!-- vi: filetype=idris2:syntax=markdown
-->
