module TypeCheck where

import Expr
import TyEnv

--
typeCheck :: Program -> IO (Either String Type)
typeCheck program = return (type_of_program program )

--
type_of_program :: Program -> Either String Type
type_of_program (Program classDecls exp) = type_of exp empty_tyenv
    
--
type_of :: Exp -> TyEnv -> Either String Type

type_of (Const_Exp n) tyenv = Right TyInt

type_of (Var_Exp var) tyenv = apply_tyenv tyenv var

type_of (Diff_Exp exp1 exp2) tyenv =
  do ty1 <- type_of exp1 tyenv 
     ty2 <- type_of exp2 tyenv 
     case ty1 of
       TyInt -> case ty2 of
                 TyInt -> Right TyInt
                 _     -> expectedButErr TyInt ty2 exp2
       _ -> expectedButErr TyInt ty1 exp1

type_of (IsZero_Exp exp1) tyenv =
  do ty1 <- type_of exp1 tyenv
     case ty1 of
       TyInt -> Right TyBool
       _     -> expectedButErr TyInt ty1 exp1

type_of exp@(If_Exp exp1 exp2 exp3) tyenv =
  do condTy <- type_of exp1 tyenv
     thenTy <- type_of exp2 tyenv
     elseTy <- type_of exp3 tyenv
     case condTy of
       TyBool -> if equalType thenTy elseTy
                 then Right thenTy
                 else inequalIfBranchTyErr thenTy elseTy exp2 exp
                      
       _      -> expectedButErr TyBool condTy exp1

type_of (Let_Exp letBindings body) tyenv = undefined

-- type_of (Let_Exp var exp1 body) tyenv =
--   do expTy  <- type_of exp1 tyenv 
--      bodyTy <- type_of body (extend_tyenv var expTy tyenv) 
--      Right bodyTy

type_of (Letrec_Exp letrecBindings letrec_body) tyenv = undefined

  -- do let tyenv1 = extend_tyenv bound_var bvar_ty
  --                   (extend_tyenv proc_name (TyFun bvar_ty ty) tyenv)
  --    procbodyTy <- type_of proc_body tyenv1
     
  --    let tyenv2 = extend_tyenv proc_name (TyFun bvar_ty ty) tyenv
  --    letrecbodyTy <- type_of letrec_body tyenv2
     
  --    if equalType ty procbodyTy
  --      then Right letrecbodyTy
  --      else expectedButErr ty procbodyTy proc_body

type_of (Proc_Exp tyVarList body) tyenv = undefined
  -- type_of (Proc_Exp var argTy body) tyenv = 
  -- do bodyTy <- type_of body (extend_tyenv var argTy tyenv)
  --    Right (TyFun argTy bodyTy)

type_of (Call_Exp rator randList) tyenv = undefined
-- type_of (Call_Exp rator rand) tyenv =
--   do funTy <- type_of rator tyenv
--      argTy <- type_of rand tyenv
--      case funTy of
--        TyFun ty1 ty2 -> if equalType ty1 argTy
--                         then Right ty2
--                         else inequalArgtyErr ty1 argTy rator rand
--        _             -> expectedFuntyButErr funTy rator

type_of _ _ = undefined         

-- 
initializeStaticClassEnv :: [ClassDecl] -> StaticClassEnv
initializeStaticClassEnv classDecls = 
  AStaticClass "object" [] [] [] []
   : map classDeclToStaticClass classDecls

classDeclToStaticClass :: ClassDecl -> StaticClass
classDeclToStaticClass (Class_Decl cname superName ifaceNames fieldTypeNames methodDecls) =
  AStaticClass superName ifaceNames fieldNames fieldTypes methodTyEnv
  where
    fieldNames = map snd fieldTypeNames
    fieldTypes = map fst fieldTypeNames
    methodTyEnv = map methodDeclToTyEnv methodDecls
classDeclToStaticClass (Interface_Decl ifaceName methodDecls) =
  AnInterface methodTyEnv
  where
    methodTyEnv = map methodDeclToTyEnv methodDecls

methodDeclToTyEnv :: MethodDecl -> (Identifier, Type)
methodDeclToTyEnv (Method_Decl ty name tyArgs _) = (name, TyFun (map fst tyArgs) ty)
methodDeclToTyEnv (AbstractMethod_Decl ty name tyArgs) = (name, TyFun (map fst tyArgs) ty) 

-- Utilities
expectedButErr expectedTy gotTy exp =
  Left $ "Expected " ++ show expectedTy ++ " but got " ++ show gotTy ++ " in " ++ show exp

expectedFuntyButErr gotTy exp =
  Left $ "Expected function type but got " ++ show gotTy ++ " in " ++ show exp

inequalIfBranchTyErr thenTy elseTy exp2 exp3 =
  Left $ "Type mismatch: \n"
          ++ "\t" ++ show thenTy ++ " in " ++ show exp2
          ++ "\t" ++ show elseTy ++ " in " ++ show exp3

inequalArgtyErr argTy1 argTy2 funexp argexp =
  Left $ "Type mismatch: \n"
          ++ "\t" ++ show argTy1 ++ " for the arugment of " ++ show funexp
          ++ "\t" ++ show argTy2 ++ " in " ++ show argexp

equalType :: Type -> Type -> Bool
equalType TyInt  TyInt  = True
equalType TyBool TyBool = True
equalType (TyFun tyList1 ty1') (TyFun tyList2 ty2') =
  equalTypes tyList1 tyList2 && equalType ty1' ty2'
equalType _ _ = False

equalTypes :: [Type] -> [Type] -> Bool
equalTypes tyList1 tyList2 = 
  and $ zipWith equalType tyList1 tyList2