module Derive.Sqlite3.AsCell

import Sqlite3.Marshall
import Sqlite3.Types
import Language.Reflection.Util

%default total

--------------------------------------------------------------------------------
--          Claims
--------------------------------------------------------------------------------

||| Top-level declaration of the `AsCell` implementation for the given data type.
export
asCellEnumImplClaim : (impl : Name) -> (p : ParamTypeInfo) -> Decl
asCellEnumImplClaim impl p = implClaim impl (implType "AsCell" p)

--------------------------------------------------------------------------------
--          Definitions
--------------------------------------------------------------------------------

x : Name
x = "x"

parameters (nms : List Name)
  encEnumClause : Con n vs -> Clause
  encEnumClause c = patClause (var c.name) `(Just ~(c.namePrim))

  decEnumClause : Con n vs -> Clause
  decEnumClause c = patClause c.namePrim `(Right ~(var c.name))

  encEnum : TypeInfo -> TTImp
  encEnum ti =
    lam (lambdaArg x) $ iCase (var x) (var ti.name) (map encEnumClause ti.cons)

  decEnum : TypeInfo -> TTImp
  decEnum ti = `(decodeJust ~(ti.namePrim) ~(dec))
    where
      catchAll : TTImp
      catchAll = `(Left $ DecodingError TEXT ~(ti.namePrim))

      dec : TTImp
      dec =
        lam (lambdaArg x) $
        iCase (var x) `(String) $
          map decEnumClause ti.cons ++ [patClause implicitTrue catchAll]

  asCellEnumDef : Name -> TypeInfo -> Decl
  asCellEnumDef f ti =
    def f [patClause (var f) `(MkAsCell TEXT ~(encEnum ti) ~(decEnum ti))]

--------------------------------------------------------------------------------
--          Deriving
--------------------------------------------------------------------------------

||| Generate declarations and implementations for `ToJSON` for a given data type
||| using default settings.
export %inline
AsCell : List Name -> ParamTypeInfo -> Res (List TopLevel)
AsCell nms p =
  case isEnum p.info of
    True  =>
      let impl := implName p "AsCell"
       in Right [ TL (asCellEnumImplClaim impl p) (asCellEnumDef nms impl p.info) ]
    False =>
      Left "Interface AsCell can only be derived for enumerations and newtypes."
