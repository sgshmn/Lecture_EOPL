module TyEnv where

import Expr(Identifier, Type)

data TyEnv = 
    Empty_tyenv
  | Extend_tyenv Identifier Type TyEnv

empty_tyenv :: TyEnv
empty_tyenv = Empty_tyenv

extend_tyenv :: Identifier -> Type -> TyEnv -> TyEnv
extend_tyenv var ty tyenv = Extend_tyenv var ty tyenv 