module Derive.Sqlite3.AsRow

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
    acc (CArg _ MW _ t) s = `(Prelude.(::) (CellType ~(ttimp vs t)) ~(s))
    acc _               s = s

||| Top-level declaration of the `AsCell` implementation for the given data type.
export
asRowImplClaim :
     (impl : Name)
  -> (p : ParamTypeInfo)
  -> (ParamCon p.numParams)
  -> Decl
asRowImplClaim impl p c =
  let tpe := var "AsRow" `app` p.applied
      pi  := piAll tpe (allImplicits p "AsCell")
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

  fromLHS : SnocList (BoundArg 2 Regular) -> TTImp
  fromLHS = foldr acc `(Data.List.Quantifiers.All.Nil)
    where
      acc : BoundArg 2 Regular -> TTImp -> TTImp
      acc (BA _ [x,_] _) t = `(Data.List.Quantifiers.All.(::) ~(bindVar x) ~(t))


  fromRHS : SnocList (BoundArg 2 Regular) -> TTImp -> TTImp
  fromRHS [<]                    res = res
  fromRHS (sx :< (BA a [x,y] _)) res = fromRHS sx (matchEither x res y)

  fromRowClause : Con n vs -> Clause
  fromRowClause c =
    let xs      := freshNames "x" c.arty
        ys      := freshNames "y" c.arty
        args    := boundArgs regular c.args [xs,ys]
        applied := appAll c.name (map (\(BA _ [_,y] _) => varStr y) args <>> [])
     in patClause (fromLHS args) (fromRHS args $ app `(Right) applied)

  to : Con n vs -> TTImp
  to c =
    lam (lambdaArg x) $ iCase (var x) implicitFalse [toRowClause c]

  from : Con n vs -> TTImp
  from c =
    lam (lambdaArg x) $ iCase (var x) implicitFalse [fromRowClause c]

  asRowDef : Name -> (rowTypes : TTImp) -> Con n vs -> Decl
  asRowDef f rowTypes c =
    def f [patClause (var f) `(MkAsRow ~(rowTypes) ~(to c) ~(from c))]

--------------------------------------------------------------------------------
--          Deriving
--------------------------------------------------------------------------------

||| Generate declarations and implementations for `AsRow` for a given
||| record type using default settings.
export
AsRow : List Name -> ParamTypeInfo -> Res (List TopLevel)
AsRow nms p =
  case (p.cons, p.info.cons) of
    ([c],[d]) =>
      let impl     := implName p "AsRow"
          rowTypes := cellTypes p.paramNames c
       in Right [ TL (asRowImplClaim impl p c) (asRowDef nms impl rowTypes d) ]
    _   => failRecord "AsRow"
