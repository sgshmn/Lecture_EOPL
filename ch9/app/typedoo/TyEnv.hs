module TyEnv where

import Expr(Identifier, Type, ClassDecl)

data TyEnv = 
    Empty_tyenv
  | Extend_tyenv Identifier Type TyEnv

empty_tyenv :: TyEnv
empty_tyenv = Empty_tyenv

extend_tyenv :: Identifier -> Type -> TyEnv -> TyEnv
extend_tyenv var ty tyenv = Extend_tyenv var ty tyenv 

apply_tyenv :: TyEnv -> Identifier -> Either String Type 
apply_tyenv Empty_tyenv var = Left $ "Variable not found: " ++ var
apply_tyenv (Extend_tyenv v ty tyenv) var
  | var == v = Right ty
  | otherwise = apply_tyenv tyenv var

type StaticClassEnv = [(Identifier, StaticClass)]

data StaticClass =
    AStaticClass {
      superName :: Maybe Identifier
    , interfaceNames :: [Identifier]
    , fieldNames :: [Identifier]
    , fieldTypes :: [Type]
    , methodTyEnv :: [(Identifier, Type)]
    }
  | AnInterface { 
      ifaceMethodTyEnv :: [(Identifier, Type)] 
    }

lookup_static_class :: StaticClassEnv -> Identifier -> StaticClass
lookup_static_class [] clzName =
  error $ clzName ++ " is not found in the static class env"
lookup_static_class ((clzName_,staticClz):clzEnv) clzName
  | clzName_ == clzName = staticClz 
  | otherwise = lookup_static_class clzEnv clzName 