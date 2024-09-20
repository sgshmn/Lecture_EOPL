module Subst(apply_subst, empty_subst, extend_subst, 
             apply_one_subst, apply_subst_to_type) where

import Expr
import TyEnv

import Data.Maybe

-- Substitutions
data Subst = 
    Empty_Subst
  | Extend_Subst Subst TypeVariable Type 
  deriving Show

apply_subst :: TypeVariable -> Subst -> Maybe Type
apply_subst tvar Empty_Subst = Nothing
apply_subst tvar (Extend_Subst subst tvar0 ty) 
  | tvar == tvar0 = Just ty
  | otherwise = apply_subst tvar subst 

empty_subst :: Subst
empty_subst = Empty_Subst  

extend_subst :: Subst -> TypeVariable -> Type -> Subst 
extend_subst subst tvar ty =
  Extend_Subst (extend_the_rest subst tvar ty) tvar ty 

extend_the_rest Empty_Subst tvar ty = 
  Empty_Subst
extend_the_rest (Extend_Subst subst tvar0 ty0) tvar ty =
  Extend_Subst (extend_the_rest subst tvar ty) 
    tvar0 (apply_one_subst ty0 tvar ty)
  
--
-- apply_one_subst t0 tv t1 = t0 [tv = t1]
apply_one_subst :: Type -> TypeVariable -> Type -> Type 
apply_one_subst TyInt tvar ty1 = TyInt 
apply_one_subst TyBool tyvar ty1 = TyBool
apply_one_subst (TyFun argTy retTy) tyvar ty1 = 
  TyFun (apply_one_subst argTy tyvar ty1)
        (apply_one_subst retTy tyvar ty1)
apply_one_subst (TyVar tvar0) tvar ty1 =
  if tvar0 == tvar then ty1 else TyVar tvar0

-- 
apply_subst_to_type :: Type -> Subst -> Type 
apply_subst_to_type TyInt subst = TyInt
apply_subst_to_type TyBool subst = TyBool
apply_subst_to_type (TyFun argTy retTy) subst =
  TyFun (apply_subst_to_type argTy subst)
        (apply_subst_to_type retTy subst)
apply_subst_to_type (TyVar tvar0) subst =
  case apply_subst tvar0 subst of
    Nothing -> TyVar tvar0
    Just ty -> ty 