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
as well as for problems and students who solved them.

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
and then we'll have a closer look at how table and column names
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
why we provide them as a list of columns (actually, it's a
heterogeneous list of type `All (TColumn t) ts`, where `t` is
the table we are currently working on, and `ts` is the list of
`SqliteType`s associated with the columns). Second, the
last two examples show how we can define `FOREIGN KEY` constraints,
and third, in `CHECK` we used an expression of type `Sqlite3.Expr.Expr`
for the first time. We will look at expressions in more detail
below, but I can already reveal that expressions are properly
typed and checked by Idris.

## Behind the Scenes: Finding Columns and their Types via String Literals

What we saw so far was very basic Idris programming: We just used
a bunch of data constructors or utilities thereof to define Idris
values representing SQL statements and commands. I'll still have to
show, how to use these to actually run the commands they represent
against a real SQLite table. But before I do that, we are going
to have a detailed look at what's going on in the background: How
does Idris resolve column names and figure out the types associated
with columns. I have looked at several libraries in other functional
languages providing DSLs for interacting with the world of relational
databases, and while many of them came with nice introductory tutorials,
none of them explained in detail how everything worked
and why these libraries were implemented the way they are.

At least in our case, the implementation is not very hard to
understand, if you know about two core concepts in Idris: Unification
and proof search.

### Looking up Table Columns

In module `Sqlite3.Table`, there is a very simple
function called `FindCol`, which tries to lookup a column name in a
list of columns and return its type. Since this might fail,
it wraps the result in a `Maybe`. But then there is a second
function called `ListColType`, which does the same thing by invoking
`FindCol` but it takes an additional proof that `FindCol`
returns a `Just` for the current column name and list of columns.

This is the core design decision behind name resolution in this
library: Instead of using an inductive type such as `Elem` for lists
for proving that a value is present in a container, we define
a lookup function that returns a `Nothing` in case the lookup fails.
This decision has two consequences, one good and the other rather
limiting: First, name resolutions mainly occurs during unification
instead of during proof search, that is, Idris will evaluate the
result of invoking `FindCol` with the current arguments, and
it will then run a very simple proof search to check
whether the result is a `Just` or not. This is
both faster than inductive proof search and it is not restricted
by the default proof search limit: We can lookup names in very
large lists of columns. For instance:

```idris
Tbl : Table
Tbl =
  table "foo" $
    map (\n => C (prim__cast_Bits8String n) INTEGER) [0..55]

test : Expr [<Tbl] INTEGER
test = "55"
```

Function `test` typechecks in a reasonable amount of time, while
it would fail with an exception in case of an inductive
proof search approach unless we would increase the proof search depth
limit before its implementation.

However, this decision can also be rather restrictive: All values
and table definitions involved *must* be known at compile time. It is
therefore assumed that the database schema has been declared and
publicly exported as in the examples above. On the other hand, it
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
a schema (a snoclist) of tables and the expression's SQLite type.
The types of the expressions on both sides of `(>)` must be identical,
because that's how the `(>)` data constructor has been defined,
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
and commands. The good new is that not a lot of complexity
is involved here: Understanding how `fromString` works for
`TColumn` as well as `Expr` - both are implemented via
the same technique - makes everything else fall into place.
In my opinion, this is quite an improvement
over languages such as Haskell, where a lot of type class
magic I still don't fully understand is
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

## Mutating the Database

In this section, we are going to look at how to add, update, and delete
data in database tables. This will require us to talk about how to
convert Idris values from and to values stored in a SQLite table,
and when we look at updating and deleting data, we will also
have to talk about SQL expressions.

### Inserting Data

Here is a first example:

```idris
insertStudent1 : (name, email : String) -> Cmd TInsert
insertStudent1 n e = INSERT Students ["name", "email"] [val n, val e]
```

As you can see, we just use the `INSERT` data constructor of `Cmd`, list
the table and columns into which we want to insert data, and add
the values to a list of values by wrapping them with `val`.

But this is of course not enough to understand what's going on, nor is
it very useful in the general case, so let us try a couple of things
to better understand and maybe even improve the code.

First and foremost, it would be important to know, if Idris performs
any sanity checks in the code above. For instance, could we insert
a numeric email address instead of a string? Let's try:

```idris
failing "Mismatch between: TEXT and INTEGER"
  insertStudent2 : String -> Bits32 -> Cmd TInsert
  insertStudent2 n e = INSERT Students ["name", "email"] [val n, val e]
```

That looks promising. Somehow, Idris figured out that if we want to insert
a value of type `Bits32`, the corresponding column type should be `INTEGER`.

### Interfaces `ToCell` and `FromCell`

The interfaces responsible for these conversions are in module
`Sqlite3.Marshall`. Interface `ToCell` describes, how to convert an
Idris value to an SQLite value. Here's an example implementation: We
are going to define an enum type for the topics of our exams:

```idris
data Topic : Type where
  OrganicChemistry   : Topic
  Cheminformatics    : Topic
  StructureAnalysis  : Topic
  MolecularModelling : Topic

%runElab derive "Topic" [Show,Eq,Ord]

ToCell Topic where
  toCellType = TEXT

  toCell OrganicChemistry   = Just "Organic Chemistry"
  toCell Cheminformatics    = Just "Cheminformatics"
  toCell StructureAnalysis  = Just "Structure Analysis"
  toCell MolecularModelling = Just "Molecular Modelling"
```

As you can see, interface `ToCell` has two functions: `toCellType` specifies
the SQLite type our values get converted to, and `toCell` describes this
conversion. The result type of `toCell` is `Maybe (IdrisType toCellType)`.
In this case, it is `Maybe String`, because `IdrisType TEXT` will return
`String` as shown in the table at the beginning of this tutorial.
Since all values in SQLite tables are theoretically nullable, we must
return a `Maybe` with `Nothing` corresponding to `NULL`.

While we are already at it, we can also write an implementation for
`FromCell Topic`, which will allow us to return values of type `Topic`
as part of our database queries:


```idris
FromCell Topic where
  fromCellType = TEXT

  fromCell =
    decodeJust "Topic" $ \case
      "Organic Chemistry"   => Right OrganicChemistry
      "Cheminformatics"     => Right Cheminformatics
      "Structure Analysis"  => Right StructureAnalysis
      "Molecular Modelling" => Right MolecularModelling
      s                     => Left (DecodingError TEXT "Unknown topic: \{s}")
```

In the implementation above, we use the `decodeJust` utility for
converting values that must not be `NULL`. You should use this most
of the time, because most of the time you might just want to use
`Maybe a` for nullable values. The result type of `fromCell` is
`Either SqlError Topic` where `SqlError` is a sum type defined in
module `Sqlite3.Types`.

On many occasions, conversions as the one above are very boring to
write, so we'd like Idris to do that for us. Like other Idris
libraries, idris2-sqlite3 provides elaborator scripts for deriving
certain marshallers automatically. For interfaces `ToCell` and
`FromCell`, this is currently possible for newtype wrappers as
well as for enumeration types, which will just use the constructor
name when being converted. Here's an example:

```idris
data Term : Type where
  SS : Term    -- spring semester
  AS : Term    -- autumn semester

%runElab derive "Term" [Show,Eq,Ord,ToCell,FromCell]

record Email where
  constructor MkEmail
  email : String

%runElab derive "Email" [Show,Eq,Ord,FromString,ToCell,FromCell]
```

Let us now try if this works as expected:

```idris
insertExam1 : Topic -> Term -> (year : Bits32) -> Cmd TInsert
insertExam1 to te y =
  INSERT Exams ["topic", "term", "year"] [val to, val te, val y]
```

Neat. Let me finish this subsection with some notes:

* Make sure to `public export` marshallers such as `ToCell` and `FromCell`
  because the results of `toCellType` and `fromCellType` must be
  known at compile time.
* One might wonder why we did not add a second parameter to the
  `ToCell` and `FromCell` interfaces instead of returning the
  SQLite from an interface function. I tried both variants, but it turned
  out that type inference works *much* better with single-parameter
  interfaces, even when only one parameter is used for interface
  resolution. In addition, single-parameter interfaces work well with the
  heterogeneous containers in `Data.List.Quantifiers`, so this spares
  us from adding some additional list-like data types for wrapping
  our `ToCell` proofs.
* At first, there was only one interface wrapping the functionality
  of both `ToCell` and `FromCell`. However, there are data types that
  are easy to convert in one direction but very hard or impossible
  to transform in the other. For instance, there are `FromCell`
  implementations for `Nat` and `Integer`, while the `ToCell`
  implementations are currently missing, because it's not clear
  how to do that without potential loss of information (unless we
  convert large integers to `TEXT` or `BLOB`, which is then somewhat
  inconsistent with the numeric type).
* I'm planning to provide derivable marshallers for refinement
  types from the [idris2-refined](https://github.com/stefan-hoeck/idris2-refined)
  library, but these are not available yet.

### Marshalling Records: Interfaces `ToRow` and `FromRow`

It would be much nicer if we could use custom Idris records
directly in our `INSERT` statements instead of having to pass
all arguments manually. Fortunately, this is possible.
Let us first define a record type for students:

```idris
record Student where
  constructor MkStudent
  name  : String
  email : Email

%runElab derive "Student" [Show,Eq]
```

The interfaces required to convert record types from and to table rows
can also be found in `Sqlite3.Marshall` and are called `FromRow` and
`ToRow`. Just like `FromCell` and `ToCell`, the come with two
functions: One for specifying the type(s) at the database side, the
other for performing the actual conversion. Here are the implementations
for `Student`:

```idris
FromRow Student where
  fromRowTypes  = [TEXT,TEXT]
  fromRow [n,e] = [| MkStudent (fromCell n) (fromCell e) |]

ToRow Student where
  toRowTypes            = [TEXT,TEXT]
  toRow (MkStudent n e) = [toCell n, toCell e]
```

As you can see, these implementations are very straight forward: We specify
a list of `SqliteType`s to represent the cell types in a data base row,
and we convert our values from and to a heterogeneous list of type
`All IdrisType fromRowTypes`, where `All` comes from `Data.List.Quantifiers` in
the base library.

Again, it is possible to derive this boring stuff automatically:

```idris
record Exam where
  constructor MkExam
  topic : Topic
  term  : Term
  year  : Bits16

%runElab derive "Exam" [Show,Eq,ToRow,FromRow]
```

With these interfaces at hand, it is much nicer to insert data into
tables with a utility function called `insert`, which is specialized
to work with Idris types with a `ToRow` implementation:

```idris
insertStudent : Student -> Cmd TInsert
insertStudent = insert Students ["name", "email"]

insertExam : Exam -> Cmd TInsert
insertExam = insert Exams ["topic", "term", "year"]
```

Again, Idris verifies for us that the types match up. The following fails:

```idris
failing "Mismatch between: TEXT and INTEGER"
  insertExam2 : Exam -> Cmd TInsert
  insertExam2 = insert Exams ["topic", "year", "term"]
```

This is not bulletproof: We could still mix up the order of `topic` and `term`
without getting an error, because both are represented as `TEXT`. The only
way out of this would be to also generate and match up table column names
from the record fields. This is currently not supported, because I'm not
sure if we always want to use our record field names as the names for
our table columns. Specifying a way to rename columns during marshalling
might help: This is what we are currently doing in the
[idris2-json](https://github.com/stefan-hoeck/idris2-json) library.
But again, this is not available yet.

### Updating and deleting Rows

The commands for updating and deleting rows in a table do not add
any additional complexity only some new syntax:

```idris
updateStudent : Bits32 -> Student -> Cmd TUpdate
updateStudent x (MkStudent n e) =
  UPDATE Students ["name" .= n, "email" .= e] ("student_id" == val x)

deleteStudent : Bits32 -> Cmd TDelete
deleteStudent x = DELETE Students ("student_id" == val x)
```

There's a new operator involved with updates, which pairs a column
name with an Idris type that converts to the correct SQLite type.
See data type `Val` in module `Sqlite3.Cmd` and the associated
operator `(.=)` for the details.

However, once thing we avoided discussing so far are SQL expressions,
although we already have seen several of them. Let's look at those
next.

### SQL Expressions

Module `Sqlite3.Expr` exports indexed data type `Expr s t`, where the
first index stands for a schema (a snoclist of tables as discussed
above) and the second for the SQLite result type of the expression
of type `SqliteType`. `Expr` is a large data type, currently
consisting of more than thirty data constructors wrapping the
core expressions we want to have at our hands when interacting
with databases.

As with other functions and data constructors we have seen so far,
operators and all-caps data constructors correspond more or less
directly to similar operators and functions in SQL, with certain
operators being more on the Idris side of things (for instance
`(&&)` instead of `AND` and `(||)` instead of `OR`
for boolean conjunctions and disjunctions).

As with other types we have seen so far, `Expr s t` comes with a
`fromString` function for referencing (possibly qualified) columns
in a schema while deriving the correct SQL type at the same time.
In addition, the usual numeric interfaces have been implemented
for `Expr`, so that typical arithmetic operators are available as
well.

Module `Sqlite3.Expr` exports function `encodeExpr` for
converting an expression to the corresponding SQL code. Note,
that this will correctly escape string literals and encode
byte vectors as strings of hexadecimal characters. However,
it is more efficient and safer to use SQL parameters as placeholders for
literals and use the C-API to set the values of all parameters.
As we will see, this is being done automatically most of the time,
so client code does not have to take care of this.

## Running SQLite Commands

To be added...

## Queries

To be added...

<!-- vi: filetype=idris2:syntax=markdown
-->
