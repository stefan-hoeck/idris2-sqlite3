module Derive.Sqlite3.ToRow

import Sqlite3.Marshall
import Sqlite3.Types
import Language.Reflection.Util

%default total

--------------------------------------------------------------------------------
--          Claims
--------------------------------------------------------------------------------

rowsTypes : Vect n Name -> ParamCon n -> TTImp
rowsTypes vs (MkParamCon _ _ args) = foldr acc `(Prelude.Nil) args
  where
    acc : ConArg n -> TTImp -> TTImp
    acc (CArg _ MW _ t) s = `(Prelude.List.(++) (ToRowTypes ~(ttimp vs t)) ~(s))
    acc _               s = s

||| Top-level declaration of the `ToCell` implementation for the given data type.
export
toRowImplClaim : (impl : Name) -> (p : ParamTypeInfo) -> Decl
toRowImplClaim impl p = implClaim impl (implType "ToRow" p)

--------------------------------------------------------------------------------
--          Definitions
--------------------------------------------------------------------------------

appList : SnocList TTImp -> TTImp
appList = foldr acc `(Data.List.Quantifiers.All.Nil)
  where
    acc : TTImp -> TTImp -> TTImp
    acc t s = `(Data.List.Quantifiers.All.(++) ~(t) ~(s))

x : Name
x = "x"

parameters (nms : List Name)

  toRowClause : Con n vs -> Clause
  toRowClause =
    accumArgs regular id appList (\(BA _ [x] _) => `(toRow ~(var x)))

  to : Con n vs -> TTImp
  to c =
    lam (lambdaArg x) $ iCase (var x) implicitFalse [toRowClause c]

  toRowDef : Name -> (rowTypes : TTImp) -> Con n vs -> Decl
  toRowDef f rowTypes c =
    def f [patClause (var f) `(MkToRow ~(rowTypes) ~(to c))]

--------------------------------------------------------------------------------
--          Deriving
--------------------------------------------------------------------------------

||| Generate declarations and implementations for `ToRow` for a given
||| record type using default settings.
export
ToRow : List Name -> ParamTypeInfo -> Res (List TopLevel)
ToRow nms p =
  case (p.cons, p.info.cons) of
    ([c],[d]) =>
      let impl     := implName p "ToRow"
          rowTypes := rowsTypes p.paramNames c
       in Right [ TL (toRowImplClaim impl p) (toRowDef nms impl rowTypes d) ]
    _   => failRecord "ToRow"
