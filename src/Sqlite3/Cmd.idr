module Sqlite3.Cmd

import Data.List.Quantifiers
import Sqlite3.Expr
import Sqlite3.Marshall
import Sqlite3.Table
import Sqlite3.Types

%default total

infix 1 .=

||| We can always convert a list of marshallable Idris values to
||| a list of SQL expressions.
export
toExprs : {ts : _} -> LAll (Maybe . IdrisType) ts -> LAll (Expr s) ts
toExprs {ts = []}    []      = []
toExprs {ts = t::ts} (v::vs) = maybe NULL (Lit t) v :: toExprs vs

||| A single name-value pair in an `UPDATE` statement.
public export
record Val (t : SQLTable) where
  constructor V
  name        : String
  {auto 0 prf : IsJust (FindCol name t.cols)}
  val         : Expr [<t] (TableColType name t)

||| Operator alias for the data constructor of `Val`,
||| specialized for usage with Idris types with a
||| `ToCell` implementation.
public export %inline
(.=) :
     {0 t        : SQLTable}
  -> {auto to    : ToCell a}
  -> (name       : String)
  -> {auto 0 prf : IsJust (FindCol name t.cols)}
  -> {auto 0 eq  : TableColType name t === ToCellType a}
  -> (val        : a)
  -> Val t
(.=) name v = V name (rewrite eq in val v)


namespace ForeignKey
  ||| Foreign Key Actions
  public export
  data Action
    = SET_NULL
    | SET_DEFAULT
    | CASCADE
    | RESTRICT
    | NO_ACTION

  ||| Foreign key constraint which supports actions.
  public export
  record Details (xs : List SqliteType ) (t : SQLTable) where
    constructor MkDetails
    target      : SQLTable
    my_cols     : LAll (TColumn t) xs
    target_cols : LAll (TColumn target) xs
    on_update   : Action
    on_delete   : Action

  ||| Add ON_UPDATE action to the given foreign key constraint.
  public export
  ON_UDPATE : Details xs t -> Action -> Details xs t
  ON_UDPATE details action = { on_update := action } details

  ||| Add ON_DELETE action to this foreign key constraint
  public export
  ON_DELETE : Details xs t -> Action -> Details xs t
  ON_DELETE details action = { on_delete := action } details


||| Column and table constraints to be used when creating a new table.
public export
data Constraint : SQLTable -> Type where
  NOT_NULL      : {0 x : _} -> TColumn t x -> Constraint t
  AUTOINCREMENT : {0 x : _} -> TColumn t x -> Constraint t
  UNIQUE        : {0 xs : _} -> LAll (TColumn t) xs -> Constraint t
  PRIMARY_KEY   : {0 xs : _} -> LAll (TColumn t) xs -> Constraint t
  ForeignKey    : {0 xs : _} -> ForeignKey.Details xs t -> Constraint t
  CHECK         : Expr [<t] BOOL -> Constraint t
  DEFAULT       :
       {0 t        : SQLTable}
    -> (s          : String)
    -> {auto 0 prf : IsJust (FindCol s t.cols)}
    -> (expr       : Expr [<t] (TableColType s t))
    -> Constraint t


||| Convenience eAPI to Construct a foreign key constraint.
public export
FOREIGN_KEY   :
     {0 xs : _}
  -> {0 t : SQLTable}
  -> (s   : SQLTable)
  -> LAll (TColumn t) xs
  -> LAll (TColumn s) xs
  -> Constraint t
FOREIGN_KEY s a b = ForeignKey $ MkDetails s a b NO_ACTION NO_ACTION



||| Index used to distinguish different types of commands.
|||
||| This should facilitate writing combinator such as
||| `IF_NOT_EXISTS` to operate only on certain commands.
public export
data CmdType : Type where
  TCreate : CmdType
  TDelete : CmdType
  TDrop   : CmdType
  TInsert : CmdType
  TSelect : CmdType
  TUpdate : CmdType

||| Data management commands for creating tables and
||| inserting, updating, or deleting rows in a table.
public export
data Cmd : CmdType -> Type where
  ||| Information used to create a new table.
  |||
  ||| It is recommended to use a combination of `CREATE_TABLE` and
  ||| `IF_NOT_EXISTS` instead of using this constructor directly.
  CreateTable :
       (t           : SQLTable)
    -> (attrs       : List (Constraint t))
    -> (ifNotExists : Bool)
    -> Cmd TCreate

  DropTable : (t : SQLTable) -> (ifExists : Bool) -> Cmd TDrop

  ||| Information required to insert data into a table.
  |||
  ||| This is well suited if you want to have full control over
  ||| the expressions used to insert data. If, however, you want
  ||| to insert an Idris value with a `ToRow` implementation,
  ||| consider using function `insert` instead.
  INSERT :
       {0 ts : List SqliteType}
    -> (t    : SQLTable)
    -> (cols : LAll (TColumn t) ts)
    -> (vals : LAll (Expr [<t]) ts)
    -> Cmd TInsert

  ||| Information required to insert or replace data in a a table.
  |||
  ||| This is like insert, but will replace values to fullfil `UNIQUE`
  ||| constraints in the table. As such, it is useful for updating
  ||| data that might not yet be present in a table.
  |||
  ||| This is well suited if you want to have full control over
  ||| the expressions used to replace data. If, however, you want
  ||| to insert an Idris value with a `ToRow` implementation,
  ||| consider using function `replace` instead.
  REPLACE :
       {0 ts : List SqliteType}
    -> (t : SQLTable)
    -> (cols : LAll (TColumn t) ts)
    -> (vals : LAll (Expr [<t]) ts)
    -> Cmd TInsert

  ||| Information required to update values in table.
  UPDATE :
       (t      : SQLTable)
    -> (set    : List (Val t))
    -> (where_ : Expr [<t] BOOL)
    -> Cmd TUpdate

  ||| Information required to delete values from a table.
  DELETE :
       (t      : SQLTable)
    -> (where_ : Expr [<t] BOOL)
    -> Cmd TDelete

||| Utility version of `INSERT` for those cases when you want to
||| insert an Idris value with a `ToRow` implementation.
export
insert :
    {auto as : ToRow v}
  ->(t       : SQLTable)
  -> LAll (TColumn t) (ToRowTypes v)
  -> (value : v)
  -> Cmd TInsert
insert t cs value = INSERT t cs (toExprs $ toRow value)

||| Utility version of `REPLACE` for those cases when you want to
||| replace an Idris value with a `ToRow` implementation.
export
replace :
    {auto as : ToRow v}
  ->(t       : SQLTable)
  -> LAll (TColumn t) (ToRowTypes v)
  -> (value : v)
  -> Cmd TInsert
replace t cs value = REPLACE t cs (toExprs $ toRow value)

namespace Cmds
  ||| A list of different types of commands, with the
  ||| command types being hidden.
  public export
  data Cmds : Type where
    Nil  : Cmds
    (::) : {0 t : _} -> Cmd t -> Cmds -> Cmds

  export
  (++) : Cmds -> Cmds -> Cmds
  []     ++ cs = cs
  (h::t) ++ cs = h :: (t ++ cs)

  export %inline
  Semigroup Cmds where (<+>) = (++)

  export %inline
  Monoid Cmds where neutral = []

  export
  fromList : List (Cmd t) -> Cmds
  fromList []      = []
  fromList (c::cs) = c :: fromList cs

||| Create a table only if it does not yet exist.
public export
IF_NOT_EXISTS : Cmd TCreate -> Cmd TCreate
IF_NOT_EXISTS (CreateTable t attrs _) = CreateTable t attrs True

||| Drop a table only if it exists.
public export
IF_EXISTS : Cmd TDrop -> Cmd TDrop
IF_EXISTS (DropTable t _) = DropTable t True

||| Convenience constructor for the `CreateTable` command
public export %inline
CREATE_TABLE : (t : SQLTable) -> List (Constraint t) -> Cmd TCreate
CREATE_TABLE t as = CreateTable t as False

||| Convenience constructor for the `DropTable` command
public export %inline
DROP_TABLE : (t : SQLTable) -> Cmd TDrop
DROP_TABLE t = DropTable t False

public export
0 JoinPred : Schema -> SQLTable -> Type
JoinPred s t = Either (List $ JColumn  s t) (Expr (s:<t) BOOL)

public export
data Join : (s : Schema) -> (t : SQLTable) -> Type where
  JOIN :
       {0 t0 : SQLTable}
    -> {0 s  : Schema}
    -> (t : SQLTable)
    -> JoinPred (s:<t0) t
    -> Join (s:<t0) t

  OUTER_JOIN :
       {0 t0 : SQLTable}
    -> {0 s  : Schema}
    -> (t : SQLTable)
    -> JoinPred (s:<t0) t
    -> Join (s:<t0) t

  CROSS_JOIN : {0 t0 : _} -> {0 s : _} -> (t : SQLTable) -> Join (s:<t0) t

  FROM : (t : SQLTable) -> Join [<] t

public export %inline
USING : (JoinPred s t -> Join s t) -> List (JColumn s t) -> Join s t
USING f = f . Left

public export %inline
ON : (JoinPred s t -> Join s t) -> Expr (s:<t) BOOL -> Join s t
ON f = f . Right

||| The `FROM` part in a select statement.
public export
data From : (s : Schema) -> Type where
  Lin  : From [<]
  (:<) : From s -> Join s t -> From (s :< t)

||| Tag indicating, whether results should be sorted in ascending
||| or descending order.
public export
data AscDesc = NoAsc | Asc | Desc

||| Different collations used during ordering.
|||
||| Currently, only `NO CASE` is supported.
public export
data Collation : (t : SqliteType) -> Type where
  None   : Collation t
  NOCASE : Collation TEXT

public export
record OrderingTerm (s : Schema) where
  constructor O
  {0 tpe : SqliteType}
  expr : Expr s tpe
  coll : Collation tpe
  asc  : AscDesc

public export %inline
ASC : Expr s t -> OrderingTerm s
ASC x = O x None Asc

public export %inline
DESC : Expr s t -> OrderingTerm s
DESC x = O x None Desc

public export %inline
COLLATE : (o : OrderingTerm s) -> Collation o.tpe -> OrderingTerm s
COLLATE o c = {coll := c} o

public export
record GroupingTerm (s : Schema) where
  constructor G
  {0 tpe : SqliteType}
  expr : Expr s tpe
  coll : Collation tpe

namespace GroupingTerm
  public export %inline
  fromString :
       {s        : Schema}
    -> (col      : String)
    -> {auto 0 p : IsJust (FindSchemaCol col s)}
    -> GroupingTerm s
  fromString col = G (fromString col) None

public export %inline
ord : GroupingTerm s -> OrderingTerm s
ord (G x c) = O x c NoAsc

public export
record NamedExpr (s : Schema) (t : SqliteType) where
  constructor AS
  expr : Expr s t
  name : String

namespace NameExpr
  public export %inline
  fromString :
       {s        : Schema}
    -> (col      : String)
    -> {auto 0 p : IsJust (FindSchemaCol col s)}
    -> NamedExpr s (SchemaColType col s)
  fromString col = Col col `AS` ""

||| Extracts the column name we use to reference a named
||| expression.
|||
||| If a custom name has been set, this will be returned. Otherwise,
||| if the expression references a table column, that columns (possibly
||| qualified) name will be returned.
export
columnName : NamedExpr s t -> String
columnName (AS (Col col) "") = col
columnName (AS _         n)  = n

||| Computes a list of named and typed columns from a list of
||| name expressions.
|||
||| Expressions with an empty string as their name are not included in
||| the result.
public export
ExprColumns : {ts : _} -> LAll (NamedExpr s) ts -> List Column
ExprColumns             []              = []
ExprColumns             (AS _ "" :: xs) = ExprColumns xs
ExprColumns {ts = t::_} (AS _ n  :: xs) = C n t :: ExprColumns xs

||| Appends an unnamed table with the list of columns coming from
||| the given list of named expressions to a schema.
|||
||| Expressions with an empty string as their name are not included in
||| the result.
|||
||| This is used to compute the full `Schema` to be used in
||| those fields of `Query` that are used for filtering, grouping, and
||| sorting.
public export
ExprSchema : {s : _} -> {ts : _} -> LAll (NamedExpr s) ts -> Schema
ExprSchema xs =
  case ExprColumns xs of
    [] => s
    cs => s :< ST "" "" cs

||| Different types of `SELECT` commands.
public export
record Query (t : Type) where
  [noHints]
  constructor Q
  {auto fromRow : FromRow t}
  distinct      : Bool
  schema        : Schema
  from          : From schema
  columns       : LAll (NamedExpr schema) (FromRowTypes t)
  where_        : Expr (ExprSchema columns) BOOL
  having        : Expr (ExprSchema columns) BOOL
  group_by      : List (GroupingTerm (ExprSchema columns))
  order_by      : List (OrderingTerm (ExprSchema columns))
  limit         : Maybe Nat
  offset        : Nat

public export %inline %hint
queryAsRow : (q : Query t) => FromRow t
queryAsRow = q.fromRow

public export
0 LQuery : List Type -> Type
LQuery = Query . HList

infixl 7 `GROUP_BY`,`ORDER_BY`,`WHERE`, `LIMIT`, `OFFSET`, `HAVING`

public export %inline
SELECT :
     {s : _}
  -> {auto fromRow : FromRow t}
  -> LAll (NamedExpr s) (FromRowTypes t)
  -> From s
  -> Query t
SELECT xs from = Q False s from xs TRUE TRUE [] [] Nothing 0

public export %inline
SELECT_DISTINCT :
     {s : _}
  -> {auto fromRow : FromRow t}
  -> LAll (NamedExpr s) (FromRowTypes t)
  -> From s
  -> Query t
SELECT_DISTINCT xs from = Q True s from xs TRUE TRUE [] [] Nothing 0

public export %inline
GROUP_BY : (q : Query t) -> List (GroupingTerm $ ExprSchema q.columns) -> Query t
GROUP_BY q gs = {group_by := gs} q

public export %inline
WHERE : (q : Query t) -> Expr (ExprSchema q.columns) BOOL -> Query t
WHERE q p = {where_ := p} q

public export %inline
HAVING : (q : Query t) -> Expr (ExprSchema q.columns) BOOL -> Query t
HAVING q p = {having := p} q

public export %inline
ORDER_BY : (q : Query t) -> List (OrderingTerm (ExprSchema q.columns)) -> Query t
ORDER_BY q os = {order_by := os} q

public export %inline
LIMIT : (q : Query t) -> Nat -> Query t
LIMIT q n = {limit := Just n} q

public export %inline
OFFSET : (q : Query t) -> Nat -> Query t
OFFSET q n = {offset := n} q
