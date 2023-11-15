module Derive.Sqlite3.FromRow

import Derive.Sqlite3.ToRow
import Sqlite3.Marshall
import Sqlite3.Types
import Language.Reflection.Util

%default total

--------------------------------------------------------------------------------
--          Claims
--------------------------------------------------------------------------------

flatTypes : Vect n Name -> ParamCon n -> TTImp
flatTypes vs (MkParamCon _ _ args) = foldr acc `(Prelude.Nil) args
  where
    acc : ConArg n -> TTImp -> TTImp
    acc (CArg _ MW _ t) s = `(Prelude.List.(++) (FromRowTypes ~(ttimp vs t)) ~(s))
    acc _               s = s

rowsTypes : Vect n Name -> ParamCon n -> TTImp
rowsTypes vs (MkParamCon _ _ args) = foldr acc `(Prelude.Nil) args
  where
    acc : ConArg n -> TTImp -> TTImp
    acc (CArg _ MW _ t) s = `(Prelude.(::) (FromRowTypes ~(ttimp vs t)) ~(s))
    acc _               s = s

||| Top-level declaration of the `FromCell` implementation for the given data type.
export
fromRowImplClaim : (impl : Name) -> (p : ParamTypeInfo) -> Decl
fromRowImplClaim impl p = implClaim impl (implType "FromRow" p)

--------------------------------------------------------------------------------
--          Definitions
--------------------------------------------------------------------------------

x : Name
x = "x"

matchEither : String -> (res : TTImp) -> String -> TTImp
matchEither x res y =
  `(case fromRow ~(varStr x) of
     Right ~(bindVar y) => ~(res)
     Left e             => Left e)

parameters (nms : List Name)
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

  from : TTImp -> Con n vs -> TTImp
  from rowTypes c =
    lam (lambdaArg x) $
      iCase
        `(splitAll ~(rowTypes) ~(var x))
        implicitFalse
        [fromRowClause c]

  fromRowDef : Name -> (ft,rt : TTImp) -> Con n vs -> Decl
  fromRowDef f ft rt c =
    def f [patClause (var f) `(MkFromRow ~(ft) ~(from rt c))]

--------------------------------------------------------------------------------
--          Deriving
--------------------------------------------------------------------------------

||| Generate declarations and implementations for `FromRow` for a given
||| record type using default settings.
export
FromRow : List Name -> ParamTypeInfo -> Res (List TopLevel)
FromRow nms p =
  case (p.cons, p.info.cons) of
    ([c],[d]) =>
      let impl     := implName p "FromRow"
          ft       := flatTypes p.paramNames c
          rt       := rowsTypes p.paramNames c
       in Right [ TL (fromRowImplClaim impl p) (fromRowDef nms impl ft rt d) ]
    _   => failRecord "FromRow"
