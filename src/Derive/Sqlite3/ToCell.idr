module Derive.Sqlite3.ToCell

import Sqlite3.Marshall
import Sqlite3.Types
import Language.Reflection.Util

%default total

--------------------------------------------------------------------------------
--          Claims
--------------------------------------------------------------------------------

cellType : Vect n Name -> ParamCon n -> Maybe TTImp
cellType vs (MkParamCon _ _ args) =
  case mapMaybe convert $ toList args of
    [t] => Just `(ToCellType ~(t))
    _   => Nothing
  where
    convert : ConArg n -> Maybe TTImp
    convert (CArg _ MW _ t) = Just $ ttimp vs t
    convert _               = Nothing

||| Top-level declaration of the `ToCell` implementation for the given
||| data type.
export
toCellImplClaim : (impl : Name) -> (p : ParamTypeInfo) -> Decl
toCellImplClaim impl p = implClaim impl (implType "ToCell" p)

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

  encNT : ParamCon n -> TTImp
  encNT c =
    lam (lambdaArg x) $ iCase (var x) implicitFalse
      [patClause `(~(var c.name) y) `(toCell y)]

  toCellNewtypeDef : Name -> TTImp -> ParamCon n -> Decl
  toCellNewtypeDef f ct c =
    def f [patClause (var f) `(MkToCell ~(ct) ~(encNT c))]

--------------------------------------------------------------------------------
--          Deriving
--------------------------------------------------------------------------------

||| Generate declarations and implementations for `ToJSON` for a given data type
||| using default settings.
export %inline
ToCell : List Name -> ParamTypeInfo -> Res (List TopLevel)
ToCell nms p =
 let impl := implName p "ToCell"
  in if isEnum p.info
        then
          Right [ TL (toCellImplClaim impl p) (toCellEnumDef nms impl p.info) ]
     else
       case p.cons of
         [c] =>
            case cellType p.paramNames c of
              Just ct => Right [ TL (toCellImplClaim impl p) (toCellNewtypeDef nms impl ct c) ]
              Nothing => Left "Interface ToCell can only be derived for enumerations and newtypes."
         _   => Left "Interface ToCell can only be derived for enumerations and newtypes."
