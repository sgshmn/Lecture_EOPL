module TypeInfer where

import Expr
import TyEnv
import Subst
import EitherState

--
typeInfer :: Exp -> IO (Either String Type)
typeInfer exp = return (Right TyInt) -- return (type_of_program exp )



-- 7.4.2 The Unifier

unifier :: Type -> Type -> Subst -> Exp -> Either_ String Subst 
unifier ty1' ty2' subst expr =
  let ty1 = apply_subst_to_type ty1' subst 
      ty2 = apply_subst_to_type ty2' subst 
  in
  if ty1 == ty2 then 
    _Right subst 
  else if isTyVar ty1 then
         if no_occurrence (typeVariable ty1) ty2
         then _Right (extend_subst subst (typeVariable ty1) ty2)
         else _Left ("No occurrence violation: " ++ 
                       show ty1 ++ ", " ++ show ty2 ++ 
                       "\n  in " ++ show expr)
  else if isTyVar ty2 then 
         if no_occurrence (typeVariable ty2) ty1
         then _Right (extend_subst subst (typeVariable ty2) ty1)
         else _Left ("No occurrence violation: " ++ 
                       show ty2 ++ ", " ++ show ty1 ++ 
                       "\n  in " ++ show expr)
  else if isTyFun ty1 && isTyFun ty2 then 
        do subst1 <- unifier (argType ty1) (argType ty2) subst expr 
           subst2 <- unifier (resType ty1) (resType ty2) subst1 expr 
           return subst2
  else _Left ("Unification failure: " ++ show ty1 ++ ", " ++ show ty2 ++
              "\n  in " ++ show expr)

no_occurrence :: TypeVariable -> Type -> Bool 
no_occurrence tyvar TyInt = True 
no_occurrence tyvar TyBool = True 
no_occurrence tyvar (TyFun argTy resTy) = 
  no_occurrence tyvar argTy && no_occurrence tyvar resTy 
no_occurrence tyvar (TyVar tyvar0) = tyvar /= tyvar0 