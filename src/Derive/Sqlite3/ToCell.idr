module Derive.Sqlite3.ToCell

import Sqlite3.Marshall
import Sqlite3.Types
import Language.Reflection.Util

%default total

--------------------------------------------------------------------------------
--          Claims
--------------------------------------------------------------------------------

||| Top-level declaration of the `ToCell` implementation for the given
||| data type.
export
toCellEnumImplClaim : (impl : Name) -> (p : ParamTypeInfo) -> Decl
toCellEnumImplClaim impl p = implClaim impl (implType "ToCell" p)

--------------------------------------------------------------------------------
--          Definitions
--------------------------------------------------------------------------------

x : Name
x = "x"

parameters (nms : List Name)
  encEnumClause : Con n vs -> Clause
  encEnumClause c = patClause (var c.name) `(Just ~(c.namePrim))

  encEnum : TypeInfo -> TTImp
  encEnum ti =
    lam (lambdaArg x) $ iCase (var x) (var ti.name) (map encEnumClause ti.cons)

  toCellEnumDef : Name -> TypeInfo -> Decl
  toCellEnumDef f ti =
    def f [patClause (var f) `(MkToCell TEXT ~(encEnum ti))]

--------------------------------------------------------------------------------
--          Deriving
--------------------------------------------------------------------------------

||| Generate declarations and implementations for `ToJSON` for a given data type
||| using default settings.
export %inline
ToCell : List Name -> ParamTypeInfo -> Res (List TopLevel)
ToCell nms p =
  case isEnum p.info of
    True  =>
      let impl := implName p "ToCell"
       in Right [ TL (toCellEnumImplClaim impl p) (toCellEnumDef nms impl p.info) ]
    False =>
      Left "Interface ToCell can only be derived for enumerations and newtypes."

