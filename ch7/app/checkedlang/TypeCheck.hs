module TypeCheck where

import qualified Data.Map as Map
import Expr

--
typeCheck :: Exp -> IO (Either String Type)
typeCheck exp = return (type_of_program exp )

--
type_of_program :: Exp -> Either String Type
type_of_program exp = type_of initTyEnv exp
     
initTyEnv :: TyEnv     
initTyEnv = Map.empty 
    
--
type TyEnv = Map.Map Identifier Type

type_of :: TyEnv -> Exp -> Either String Type

type_of tyenv exp@(Const_Exp n) = return TyInt

type_of tyenv exp@(Var_Exp var) =
  case Map.lookup var tyenv of
    Just ty -> return ty
    Nothing -> type_error $ "Variable not found: " ++ var

type_of tyenv exp@(Diff_Exp exp1 exp2) =
  do ty1 <- type_of tyenv exp1
     ty2 <- type_of tyenv exp2
     case ty1 of
       TyInt -> case ty2 of
                 TyInt -> return TyInt
                 _     -> expectedButErr TyInt ty2 exp2
       _ -> expectedButErr TyInt ty1 exp1

type_of tyenv exp@(IsZero_Exp exp1) =
  do ty1 <- type_of tyenv exp1
     case ty1 of
       TyInt -> return TyBool
       _     -> expectedButErr TyInt ty1 exp1

type_of tyenv exp@(If_Exp exp1 exp2 exp3) =
  do condTy <- type_of tyenv exp1
     thenTy <- type_of tyenv exp2
     elseTy <- type_of tyenv exp3
     case condTy of
       TyBool -> if equalType thenTy elseTy
                 then return thenTy
                 else inequalTypeErr thenTy elseTy exp2 exp
                      
       _      -> expectedButErr TyBool condTy exp1

type_of tyenv exp@(Let_Exp var exp1 body) =
  do expTy  <- type_of tyenv exp1
     bodyTy <- type_of (Map.insert var expTy tyenv) body
     return bodyTy

type_of tyenv exp@(Letrec_Exp ty proc_name bound_var bvar_ty proc_body letrec_body) =
  do let tyenv1 = Map.insert bound_var bvar_ty $ Map.insert proc_name (TyFun bvar_ty ty) $ tyenv
     procbodyTy <- type_of tyenv1 proc_body
     
     let tyenv2 = Map.insert proc_name (TyFun bvar_ty ty) tyenv
     letrecbodyTy <- type_of tyenv2 letrec_body
     
     if equalType ty procbodyTy
       then return letrecbodyTy
       else expectedButErr ty procbodyTy proc_body

type_of tyenv exp@(Proc_Exp var argTy body) =
  do bodyTy <- type_of (Map.insert var argTy tyenv) body
     return (TyFun argTy bodyTy)

type_of tyenv exp@(Call_Exp rator rand) =
  do funTy <- type_of tyenv rator
     argTy <- type_of tyenv rand
     case funTy of
       TyFun ty1 ty2 -> if equalType ty1 argTy
                        then return ty2
                        else inequalArgtyErr ty1 argTy rator rand
       _             -> expectedFuntyButErr funTy rator

         

-- Utilities
type_error :: String -> Either String Type
type_error msg = Left msg

expectedButErr expectedTy gotTy exp =
  type_error $ "Expected " ++ show expectedTy ++ " but got " ++ show gotTy ++ " in " ++ show exp

expectedFuntyButErr gotTy exp =
  type_error $ "Expected function type but got " ++ show gotTy ++ " in " ++ show exp

inequalTypeErr thenTy elseTy exp2 exp3 =
  type_error $ "Type mismatch: \n"
          ++ "\t" ++ show thenTy ++ " in " ++ show exp2
          ++ "\t" ++ show elseTy ++ " in " ++ show exp3

inequalArgtyErr argTy1 argTy2 funexp argexp =
  type_error $ "Type mismatch: \n"
          ++ "\t" ++ show argTy1 ++ " for the arugment of " ++ show funexp
          ++ "\t" ++ show argTy2 ++ " in " ++ show argexp

equalType :: Type -> Type -> Bool
equalType TyInt  TyInt  = True
equalType TyBool TyBool = True
equalType (TyFun ty1 ty1') (TyFun ty2 ty2') =
  equalType ty1 ty2 && equalType ty1' ty2'
equalType _ _ = False

