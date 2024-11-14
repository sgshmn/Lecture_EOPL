{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module TypeCheck where

import Expr
import TyEnv
import EnvStore (DenVal(SelfObject_Val))

--
typeCheck :: Program -> IO (Either String Type)
typeCheck program = return (type_of_program program )

--
type_of_program :: Program -> Either String Type
type_of_program (Program classDecls exp) =
  let clzEnv = initializeStaticClassEnv classDecls
  in  type_of clzEnv exp empty_tyenv

--
type_of :: StaticClassEnv -> Exp -> TyEnv -> Either String Type

type_of clzEnv (Const_Exp n) tyenv = Right TyInt

type_of clzEnv (Var_Exp var) tyenv = apply_tyenv tyenv var

type_of clzEnv (Diff_Exp exp1 exp2) tyenv =
  do ty1 <- type_of clzEnv exp1 tyenv
     ty2 <- type_of clzEnv exp2 tyenv
     case ty1 of
       TyInt -> case ty2 of
                 TyInt -> Right TyInt
                 _     -> expectedButErr TyInt ty2 exp2
       _ -> expectedButErr TyInt ty1 exp1

type_of clzEnv (Sum_Exp exp1 exp2) tyenv =
  do ty1 <- type_of clzEnv exp1 tyenv
     ty2 <- type_of clzEnv exp2 tyenv
     case ty1 of
       TyInt -> case ty2 of
                 TyInt -> Right TyInt
                 _     -> expectedButErr TyInt ty2 exp2
       _ -> expectedButErr TyInt ty1 exp1

type_of clzEnv (IsZero_Exp exp1) tyenv =
  do ty1 <- type_of clzEnv exp1 tyenv
     case ty1 of
       TyInt -> Right TyBool
       _     -> expectedButErr TyInt ty1 exp1

type_of clzEnv exp@(If_Exp exp1 exp2 exp3) tyenv =
  do condTy <- type_of clzEnv exp1 tyenv
     thenTy <- type_of clzEnv exp2 tyenv
     elseTy <- type_of clzEnv exp3 tyenv
     case condTy of
       TyBool -> if equalType thenTy elseTy
                 then Right thenTy
                 else inequalIfBranchTyErr thenTy elseTy exp2 exp

       _      -> expectedButErr TyBool condTy exp1

type_of clzEnv (Let_Exp letBindings body) tyenv = undefined

-- type_of (Let_Exp var exp1 body) tyenv =
--   do expTy  <- type_of exp1 tyenv 
--      bodyTy <- type_of body (extend_tyenv var expTy tyenv) 
--      Right bodyTy

type_of clzEnv (Letrec_Exp letrecBindings letrec_body) tyenv = undefined

  -- do let tyenv1 = extend_tyenv bound_var bvar_ty
  --                   (extend_tyenv proc_name (TyFun bvar_ty ty) tyenv)
  --    procbodyTy <- type_of proc_body tyenv1

  --    let tyenv2 = extend_tyenv proc_name (TyFun bvar_ty ty) tyenv
  --    letrecbodyTy <- type_of letrec_body tyenv2

  --    if equalType ty procbodyTy
  --      then Right letrecbodyTy
  --      else expectedButErr ty procbodyTy proc_body

type_of clzEnv (Proc_Exp tyVarList body) tyenv = undefined
  -- type_of (Proc_Exp var argTy body) tyenv = 
  -- do bodyTy <- type_of body (extend_tyenv var argTy tyenv)
  --    Right (TyFun argTy bodyTy)

type_of clzEnv (Call_Exp rator randList) tyenv = undefined
-- type_of (Call_Exp rator rand) tyenv =
--   do funTy <- type_of rator tyenv
--      argTy <- type_of rand tyenv
--      case funTy of
--        TyFun ty1 ty2 -> if equalType ty1 argTy
--                         then Right ty2
--                         else inequalArgtyErr ty1 argTy rator rand
--        _             -> expectedFuntyButErr funTy rator

type_of clzEnv (Block_Exp expList) tyenv = undefined

type_of clzEnv (Set_Exp var exp) tyenv = undefined

type_of clzEnv (List_Exp expList) tyenv = undefined

type_of clzEnv (New_Object_Exp cname expList) tyenv = undefined

type_of clzEnv (Method_Call_Exp exp1 mname expList) tyenv = undefined

type_of clzEnv (Super_Call_Exp mname expList) tyenv = undefined

type_of clzEnv Self_Exp tyenv = apply_tyenv tyenv self

type_of clzEnv (Cast_Exp exp cname) tyenv =
  do objTy <- type_of clzEnv exp tyenv
     case objTy of 
      TyClass _ -> Right objTy 
      _ -> expectedButErr (TyClass "...") objTy exp

type_of clzEnv (InstanceOf_Exp exp cname) tyenv =
  do objTy <- type_of clzEnv exp tyenv
     case objTy of 
      TyClass _ -> Right TyBool
      _ -> expectedButErr (TyClass "...") objTy exp

type_of_call :: Type -> [Type] -> [Exp] -> Exp -> Either String Type
type_of_call (TyFun _argTyList _resTy) argTyList argList exp
  | length _argTyList == length argTyList =
      do type_of_args _argTyList argTyList argList
         Right _resTy
      
  | otherwise = wrongNumberOfArgsErr _argTyList argTyList exp
type_of_call funTy _ _ exp = expectedFuntyButErr funTy exp

type_of_args :: [Type] -> [Type] -> [Exp] -> Either String ()
type_of_args [] [] [] = Right ()
type_of_args (randTy:randTys) (argTy:argTys) (rand:rands) = 
  do check_is_subtype randTy argTy rand
     type_of_args randTys argTys rands 
    
check_is_subtype :: Type -> Type -> Exp -> Either String ()
check_is_subtype randTy argTy exp = 
  if is_subtype randTy argTy then Right () 
  else subtypeFailure randTy argTy exp

is_subtype :: Type -> Type -> Bool
is_subtype (TyClass clzName1) (TyClass clzName2) = 
  statically_is_subclass clzName1 clzName2
is_subtype (TyFun argTys1 resTy1) (TyFun argTys2 resTy2) = 
  is_subtype_list argTys1 argTys2 && is_subtype resTy2 resTy1 
is_subtype ty1 ty2 = equalType ty1 ty2

is_subtype_list :: [Type] -> [Type] -> Bool
is_subtype_list [] [] = True
is_subtype_list (ty1:tys1) (ty2:tys2) = 
  is_subtype ty1 ty2 && is_subtype_list tys1 tys2 

statically_is_subclass :: Identifier -> Identifier -> Bool
statically_is_subclass clzName1 clzName2 = 
  if clzName1 == clzName2 then True
  else 
    let maybeSuperName1 = undefined in 
      case maybeSuperName1 of
        Just superName1 -> statically_is_subclass superName1 clzName2 
        Nothing -> 
          let maybeIfaceNames1 = undefined :: Maybe [Identifier] in 
            case maybeIfaceNames1 of
              Just ifaceNames1 -> elem clzName2 ifaceNames1 
              Nothing -> False  

-- Static Class Environment
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

wrongNumberOfArgsErr _argTyList argTyList exp =
  Left $ "Wrong number of arguments: \n"
          ++ "\t" ++ show _argTyList ++ "\n"
          ++ "\t" ++ show argTyList ++ " in " ++ show exp      

subtypeFailure randTy argTy exp =
  Left $ "Subtype failure: \n"
          ++ "\t" ++ show randTy ++ " is not a subtype of\n"
          ++ "\t" ++ show argTy ++ " in " ++ show exp                

equalType :: Type -> Type -> Bool
equalType TyInt  TyInt  = True
equalType TyBool TyBool = True
equalType (TyFun tyList1 ty1') (TyFun tyList2 ty2') =
  equalTypes tyList1 tyList2 && equalType ty1' ty2'
equalType _ _ = False

equalTypes :: [Type] -> [Type] -> Bool
equalTypes tyList1 tyList2 =
  and $ zipWith equalType tyList1 tyList2