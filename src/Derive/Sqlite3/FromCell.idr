module Derive.Sqlite3.FromCell

import Sqlite3.Marshall
import Sqlite3.Types
import Language.Reflection.Util

%default total

--------------------------------------------------------------------------------
--          Claims
--------------------------------------------------------------------------------

||| Top-level declaration of the `FromCell` implementation
||| for the given data type.
export
fromCellEnumImplClaim : (impl : Name) -> (p : ParamTypeInfo) -> Decl
fromCellEnumImplClaim impl p = implClaim impl (implType "FromCell" p)

--------------------------------------------------------------------------------
--          Definitions
--------------------------------------------------------------------------------

x : Name
x = "x"

parameters (nms : List Name)
  decEnumClause : Con n vs -> Clause
  decEnumClause c = patClause c.namePrim `(Right ~(var c.name))

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

  fromCellEnumDef : Name -> TypeInfo -> Decl
  fromCellEnumDef f ti =
    def f [patClause (var f) `(MkFromCell TEXT ~(decEnum ti))]

--------------------------------------------------------------------------------
--          Deriving
--------------------------------------------------------------------------------

||| Generate declarations and implementations for `ToJSON` for a given data type
||| using default settings.
export %inline
FromCell : List Name -> ParamTypeInfo -> Res (List TopLevel)
FromCell nms p =
  case isEnum p.info of
    True  =>
      let impl := implName p "FromCell"
       in Right [ TL (fromCellEnumImplClaim impl p) (fromCellEnumDef nms impl p.info) ]
    False =>
      Left "Interface FromCell can only be derived for enumerations and newtypes."
