module Derive.Sqlite3.ToRow

import Sqlite3.Marshall
import Sqlite3.Types
import Language.Reflection.Util

%default total

--------------------------------------------------------------------------------
--          Claims
--------------------------------------------------------------------------------

cellTypes : Vect n Name -> ParamCon n -> TTImp
cellTypes vs (MkParamCon _ _ args) = foldr acc `(Prelude.Nil) args
  where
    acc : ConArg n -> TTImp -> TTImp
    acc (CArg _ MW _ t) s = `(Prelude.(::) (ToCellType ~(ttimp vs t)) ~(s))
    acc _               s = s

||| Top-level declaration of the `ToCell` implementation for the given data type.
export
toRowImplClaim :
     (impl : Name)
  -> (p : ParamTypeInfo)
  -> (ParamCon p.numParams)
  -> Decl
toRowImplClaim impl p c =
  let tpe := var "ToRow" `app` p.applied
      pi  := piAll tpe (allImplicits p "ToCell")
   in implClaim impl pi

--------------------------------------------------------------------------------
--          Definitions
--------------------------------------------------------------------------------

appList : SnocList TTImp -> TTImp
appList = foldr acc `(Data.List.Quantifiers.All.Nil)
  where
    acc : TTImp -> TTImp -> TTImp
    acc t s = `(Data.List.Quantifiers.All.(::) ~(t) ~(s))

x : Name
x = "x"

matchEither : String -> (res : TTImp) -> String -> TTImp
matchEither x res y =
  `(case fromCell ~(varStr x) of
     Right ~(bindVar y) => ~(res)
     Left e             => Left e)

parameters (nms : List Name)

  toRowClause : Con n vs -> Clause
  toRowClause =
    accumArgs regular id appList (\(BA _ [x] _) => `(toCell ~(varStr x)))

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
          rowTypes := cellTypes p.paramNames c
       in Right [ TL (toRowImplClaim impl p c) (toRowDef nms impl rowTypes d) ]
    _   => failRecord "ToRow"

