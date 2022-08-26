module TypeCheck where

import qualified Data.Map as Map
import Expr

import Prelude hiding (error)

--
typeCheck :: Exp -> IO (Either String Type)
typeCheck exp =
  case tcExpr Map.empty exp of
    Left errMsg -> return (Left errMsg)
    Right ty    -> return (Right ty)
     

type TyEnv = Map.Map Identifier Type

tcExpr :: TyEnv -> Exp -> Either String Type

tcExpr tyenv exp@(Const_Exp n) = return TyInt

tcExpr tyenv exp@(Var_Exp var) =
  case Map.lookup var tyenv of
    Just ty -> return ty
    Nothing -> error $ "Variable not found: " ++ var

tcExpr tyenv exp@(Diff_Exp exp1 exp2) =
  do ty1 <- tcExpr tyenv exp1
     ty2 <- tcExpr tyenv exp2
     case ty1 of
       TyInt -> case ty2 of
                 TyInt -> return TyInt
                 _     -> expectedButErr TyInt ty2 exp2
       _ -> expectedButErr TyInt ty1 exp1

tcExpr tyenv exp@(IsZero_Exp exp1) =
  do ty1 <- tcExpr tyenv exp1
     case ty1 of
       TyInt -> return TyBool
       _     -> expectedButErr TyInt ty1 exp1

tcExpr tyenv exp@(If_Exp exp1 exp2 exp3) =
  do condTy <- tcExpr tyenv exp1
     thenTy <- tcExpr tyenv exp2
     elseTy <- tcExpr tyenv exp3
     case condTy of
       TyBool -> if equalType thenTy elseTy
                 then return thenTy
                 else inequalErr thenTy elseTy exp2 exp
                      
       _      -> expectedButErr TyBool condTy exp1

tcExpr tyenv exp@(Let_Exp var exp1 body) =
  do expTy  <- tcExpr tyenv exp1
     bodyTy <- tcExpr (Map.insert var expTy tyenv) body
     return bodyTy

tcExpr tyenv exp@(Letrec_Exp ty proc_name bound_var bvar_ty proc_body letrec_body) =
  do let tyenv1 = Map.insert bound_var bvar_ty $ Map.insert proc_name (TyFun bvar_ty ty) $ tyenv
     procbodyTy <- tcExpr tyenv1 proc_body
     
     let tyenv2 = Map.insert proc_name (TyFun bvar_ty ty) tyenv
     letrecbodyTy <- tcExpr tyenv2 letrec_body
     
     if equalType ty procbodyTy
       then return letrecbodyTy
       else expectedButErr ty procbodyTy proc_body

tcExpr tyenv exp@(Proc_Exp var argTy body) =
  do bodyTy <- tcExpr (Map.insert var argTy tyenv) body
     return (TyFun argTy bodyTy)

tcExpr tyenv exp@(Call_Exp rator rand) =
  do funTy <- tcExpr tyenv rator
     argTy <- tcExpr tyenv rand
     case funTy of
       TyFun ty1 ty2 -> if equalType ty1 argTy
                        then return ty2
                        else inequalArgtyErr ty1 argTy rator rand
       _             -> expectedFuntyButErr funTy rator

         

-- Utilities
error :: String -> Either String Type
error msg = Left msg

expectedButErr expectedTy gotTy exp =
  error $ "Expected " ++ show expectedTy ++ " but got " ++ show gotTy ++ " in " ++ show exp

expectedFuntyButErr gotTy exp =
  error $ "Expected function type but got " ++ show gotTy ++ " in " ++ show exp

inequalErr thenTy elseTy exp2 exp3 =
  error $ "Type mismatch: \n"
          ++ "\t" ++ show thenTy ++ " in " ++ show exp2
          ++ "\t" ++ show elseTy ++ " in " ++ show exp3

inequalArgtyErr argTy1 argTy2 funexp argexp =
  error $ "Type mismatch: \n"
          ++ "\t" ++ show argTy1 ++ " for the arugment of " ++ show funexp
          ++ "\t" ++ show argTy2 ++ " in " ++ show argexp

equalType :: Type -> Type -> Bool
equalType TyInt  TyInt  = True
equalType TyBool TyBool = True
equalType (TyFun ty1 ty1') (TyFun ty2 ty2') =
  equalType ty1 ty2 && equalType ty1' ty2'
equalType _ _ = False

