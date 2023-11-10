module Derive.Sqlite3.FromCell

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
    [t] => Just `(FromCellType ~(t))
    _   => Nothing
  where
    convert : ConArg n -> Maybe TTImp
    convert (CArg _ MW _ t) = Just $ ttimp vs t
    convert _               = Nothing

||| Top-level declaration of the `FromCell` implementation
||| for the given data type.
export
fromCellImplClaim : (impl : Name) -> (p : ParamTypeInfo) -> Decl
fromCellImplClaim impl p = implClaim impl (implType "FromCell" p)

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

  decNewtype : ParamCon n -> TTImp
  decNewtype c = `(map ~(var c.name) . fromCell)

  fromCellNewtypeDef : Name -> TTImp -> ParamCon n -> Decl
  fromCellNewtypeDef f ct c =
    def f [patClause (var f) `(MkFromCell ~(ct) ~(decNewtype c))]

--------------------------------------------------------------------------------
--          Deriving
--------------------------------------------------------------------------------

||| Generate declarations and implementations for `ToJSON` for a given data type
||| using default settings.
export %inline
FromCell : List Name -> ParamTypeInfo -> Res (List TopLevel)
FromCell nms p =
 let impl := implName p "FromCell"
  in if isEnum p.info
        then
          Right [ TL (fromCellImplClaim impl p) (fromCellEnumDef nms impl p.info) ]
     else
       case p.cons of
         [c] =>
            case cellType p.paramNames c of
              Just ct => Right [ TL (fromCellImplClaim impl p) (fromCellNewtypeDef nms impl ct c) ]
              Nothing => Left "Interface FromCell can only be derived for enumerations and newtypes."
         _   => Left "Interface FromCell can only be derived for enumerations and newtypes."
