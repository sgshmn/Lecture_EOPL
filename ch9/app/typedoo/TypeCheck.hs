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

type_of clzEnv exp@(New_Object_Exp cname expList) tyenv =
  do argTys <- types_of_exps clzEnv expList tyenv
     case lookup_static_class clzEnv cname of 
        Nothing -> Left $ "Class " ++ cname ++ " is not found in " ++ show exp
        Just (AStaticClass _ _ _ _ mtyenv) -> 
          do mty <- find_method_type clzEnv cname initialize 
             type_of_call clzEnv mty argTys expList exp
        Just (AnInterface _) -> Left $ "Cannot instantiate an interface: " ++ cname

type_of clzEnv exp@(Method_Call_Exp exp1 mname expList) tyenv =
  do argTys <- types_of_exps clzEnv expList tyenv 
     objTy <- type_of clzEnv exp1 tyenv 
     case objTy of  -- Note: no check like this in the EOPL book
       TyClass clzName ->
         do mty <- find_method_type clzEnv clzName mname
            type_of_call clzEnv mty argTys expList exp
       _ -> expectedClasstyButErr objTy exp 

type_of clzEnv exp@(Super_Call_Exp mname expList) tyenv =
  do argTys <- types_of_exps clzEnv expList tyenv 
     objTy <- apply_tyenv tyenv self
     case objTy of  -- Note: no check like this in the EOPL book
       TyClass clzName ->
         do mty <- find_method_type clzEnv clzName mname
            type_of_call clzEnv mty argTys expList exp
       _ -> expectedClasstyButErr objTy exp 

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

types_of_exps :: StaticClassEnv -> [Exp] -> TyEnv -> Either String [Type]
types_of_exps clzEnv [] tyenv = Right []
types_of_exps clzEnv (exp:exps) tyenv = 
  do ty <- type_of clzEnv exp tyenv 
     tys <- types_of_exps clzEnv exps tyenv 
     Right (ty:tys)

type_of_call :: StaticClassEnv -> Type -> [Type] -> [Exp] -> Exp -> Either String Type
type_of_call clzEnv (TyFun _argTyList _resTy) argTyList argList exp
  | length _argTyList == length argTyList =
      do type_of_args clzEnv _argTyList argTyList argList
         Right _resTy
      
  | otherwise = wrongNumberOfArgsErr _argTyList argTyList exp
type_of_call clzEnv funTy _ _ exp = expectedFuntyButErr funTy exp

type_of_args :: StaticClassEnv -> [Type] -> [Type] -> [Exp] -> Either String ()
type_of_args clzEnv [] [] [] = Right ()
type_of_args clzEnv (randTy:randTys) (argTy:argTys) (rand:rands) = 
  do check_is_subtype clzEnv randTy argTy rand
     type_of_args clzEnv randTys argTys rands 
type_of_args clzEnv randTys argTys exps = 
  wrongNumberOfArgsErr3 randTys argTys exps
    
check_is_subtype :: StaticClassEnv -> Type -> Type -> Exp -> Either String ()
check_is_subtype clzEnv randTy argTy exp = 
  if is_subtype clzEnv randTy argTy then Right () 
  else subtypeFailure randTy argTy exp

is_subtype :: StaticClassEnv -> Type -> Type -> Bool
is_subtype clzEnv (TyClass clzName1) (TyClass clzName2) = 
  statically_is_subclass clzEnv clzName1 clzName2
is_subtype clzEnv (TyFun argTys1 resTy1) (TyFun argTys2 resTy2) = 
  is_subtype_list clzEnv argTys1 argTys2 
    && is_subtype clzEnv resTy2 resTy1 
is_subtype clzEnv ty1 ty2 = equalType ty1 ty2

is_subtype_list :: StaticClassEnv -> [Type] -> [Type] -> Bool
is_subtype_list clzEnv [] [] = True
is_subtype_list clzEnv (ty1:tys1) (ty2:tys2) = 
  is_subtype clzEnv ty1 ty2 
    && is_subtype_list clzEnv tys1 tys2
is_subtype_list clzEnv _ _ = False    

statically_is_subclass :: StaticClassEnv -> Identifier -> Identifier -> Bool
statically_is_subclass clzEnv clzName1 clzName2 = 
  clzName1 == clzName2 ||
    case lookup_static_class clzEnv clzName1 of
      Nothing -> False
      Just (AStaticClass maybeSuperName1 ifaceNames1 fs ftys mtyenv) ->
            case maybeSuperName1 of
              Just superName1 -> statically_is_subclass clzEnv superName1 clzName2 
              Nothing -> clzName2 `elem` ifaceNames1
           
      Just (AnInterface absmdecls) -> False 
         -- Note: interfaces have no inheritance relationship in TYPED-OO.  

-- Static Class Environment
initializeStaticClassEnv :: [ClassDecl] -> StaticClassEnv
initializeStaticClassEnv classDecls =
  ("object", AStaticClass Nothing [] [] [] [])
    : map classDeclToStaticClass classDecls

classDeclToStaticClass :: ClassDecl -> (Identifier, StaticClass)
classDeclToStaticClass (Class_Decl cname superName ifaceNames fieldTypeNames methodDecls) =
  (cname, AStaticClass (Just superName) ifaceNames fieldNames fieldTypes methodTyEnv)
  where
    fieldNames = map snd fieldTypeNames
    fieldTypes = map fst fieldTypeNames
    methodTyEnv = map methodDeclToTyEnv methodDecls
classDeclToStaticClass (Interface_Decl ifaceName methodDecls) =
 (ifaceName, AnInterface methodTyEnv)
  where
    methodTyEnv = map methodDeclToTyEnv methodDecls

methodDeclToTyEnv :: MethodDecl -> (Identifier, Type)
methodDeclToTyEnv (Method_Decl ty name tyArgs _) = (name, TyFun (map fst tyArgs) ty)
methodDeclToTyEnv (AbstractMethod_Decl ty name tyArgs) = (name, TyFun (map fst tyArgs) ty)

-- 
check_class_decl :: StaticClassEnv -> ClassDecl -> Either String ()
check_class_decl clzEnv (Class_Decl cname superName ifaceNames fieldTypeNames methodDecls) =
  case lookup_static_class clzEnv cname of
    Nothing -> Left $ "Class " ++ cname ++ " is not found"
    Just sc -> 
      let fieldNames = map snd fieldTypeNames 
          fieldTypes = map fst fieldTypeNames
      in do check_method_decls clzEnv cname superName fieldNames fieldTypes methodDecls
            check_if_implements clzEnv cname ifaceNames 
check_class_decl clzEnv (Interface_Decl ifaceName methodDecls) = Right ()

check_method_decls :: StaticClassEnv -> Identifier -> Identifier -> [Identifier] -> [Type] -> [MethodDecl] -> Either String ()
check_method_decls clzEnv cname superName fieldNames fieldTypes [] = Right ()
check_method_decls clzEnv cname superName fieldNames fieldTypes (methodDecl:methodDecls) =
  do check_method_decl clzEnv cname superName fieldNames fieldTypes methodDecl
     check_method_decls clzEnv cname superName fieldNames fieldTypes methodDecls

check_method_decl :: StaticClassEnv -> Identifier -> Identifier -> [Identifier] -> [Type] -> MethodDecl -> Either String ()
check_method_decl clzEnv cname superName fieldNames fieldTypes (Method_Decl ty name tyArgs body) =
  let vars   = map snd tyArgs
      varTys = map fst tyArgs
      tyenv = extend_tyenv_with vars varTys 
                (extend_tyenv_with_self_and_super (TyClass cname) superName 
                  (extend_tyenv_with fieldNames fieldTypes empty_tyenv)) in
    do bodyTy <- type_of clzEnv body tyenv
       check_is_subtype clzEnv bodyTy ty body
       if name == initialize then Right ()
       else
         case lookup_static_class clzEnv superName of
           Nothing -> Right () 
           Just (AStaticClass _ _ _ _ mtyenv) -> 
             case lookup name mtyenv of 
               Just mty -> check_is_subtype clzEnv ty mty body
               Nothing -> Right ()
           Just (AnInterface mtyenv) ->
             case lookup name mtyenv of 
               Just mty -> check_is_subtype clzEnv ty mty body
               Nothing -> Right ()

check_method_decl clzEnv cname superName fieldNames fieldTypes (AbstractMethod_Decl ty name tyArgs) =
  Right () -- Deadcode: Abstract methods never appear in class definitions in TYPED-OO.

check_if_implements :: StaticClassEnv -> Identifier -> [Identifier] -> Either String ()
check_if_implements clzEnv cname [] = Right ()
check_if_implements clzEnv cname (iname:inames) =
  do check_if_implements_ clzEnv cname iname 
     check_if_implements clzEnv cname inames

check_if_implements_ :: StaticClassEnv -> Identifier -> Identifier -> Either String ()
check_if_implements_ clzEnv cname iname =
  case lookup_static_class clzEnv iname of
    Nothing -> Left $ "Interface " ++ iname ++ " is not found"
    Just (AnInterface iface_mtyenv) -> 
      case lookup_static_class clzEnv cname of
        Nothing -> Left $ "Class " ++ cname ++ " is not found"
        Just (AStaticClass _ _ _ _ clz_mtyenv) -> 
          check_mth_impl clzEnv iface_mtyenv clz_mtyenv
        Just (AnInterface _) -> 
          Left $ "Interface " ++ cname ++ " cannot implement another interface " ++ iname
    Just (AStaticClass _ _ _ _ _) -> 
      Left $ "class " ++ cname ++ " attempts to implement non-interface " ++ iname

check_mth_impl :: StaticClassEnv -> [(Identifier,Type)] -> [(Identifier,Type)] -> Either String ()
check_mth_impl clzEnv [] clz_mtyenv = Right ()
check_mth_impl clzEnv ((mname, iface_mty):iface_mtyenv) clz_mtyenv =
  case lookup mname clz_mtyenv of
    Just clz_mty -> 
      do check_is_subtype clzEnv clz_mty iface_mty (Var_Exp mname) -- Todo: The exp is ad-hoc!
         check_mth_impl clzEnv iface_mtyenv clz_mtyenv
    Nothing -> 
      Left $ "class attempts to implement missing method " ++ mname


-- Utilities
expectedButErr expectedTy gotTy exp =
  Left $ "Expected " ++ show expectedTy ++ " but got " ++ show gotTy ++ " in " ++ show exp

expectedFuntyButErr gotTy exp =
  Left $ "Expected function type but got " ++ show gotTy ++ " in " ++ show exp

expectedClasstyButErr gotTy exp =
  Left $ "Expected class type but got " ++ show gotTy ++ " in " ++ show exp

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


wrongNumberOfArgsErr3 _argTyList argTyList exps =
  Left $ "Wrong number of arguments: \n"
          ++ "\t" ++ show _argTyList ++ "\n"
          ++ "\t" ++ show argTyList ++ "\n" 
          ++ "\t" ++ show exps             

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