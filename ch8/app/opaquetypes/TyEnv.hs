{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module TyEnv where

import Expr(Identifier, Type, Interface(..), Declaration(..))

-- Invariant: Types in type environments are always fully expanded. 
data TyEnv = 
    Empty_tyenv
  | Extend_tyenv Identifier Type TyEnv
  | Extend_tyenv_with_module Identifier Interface TyEnv
  | Extend_tyenv_with_type Identifier Type TyEnv

empty_tyenv :: TyEnv
empty_tyenv = Empty_tyenv

extend_tyenv :: Identifier -> Type -> TyEnv -> TyEnv
extend_tyenv var ty tyenv = Extend_tyenv var ty tyenv 

extend_tyenv_with_module :: Identifier -> Interface -> TyEnv -> TyEnv
extend_tyenv_with_module mod_var iface tyenv = 
  Extend_tyenv_with_module mod_var iface tyenv

extend_tyenv_with_type :: Identifier -> Type -> TyEnv -> TyEnv
extend_tyenv_with_type tname ty tyenv = 
  Extend_tyenv_with_type tname ty tyenv

apply_tyenv :: TyEnv -> Identifier -> Either String Type 
apply_tyenv Empty_tyenv var = Left $ "Variable not found: " ++ var
apply_tyenv (Extend_tyenv v ty tyenv) var
  | var == v = Right ty
  | otherwise = apply_tyenv tyenv var
apply_tyenv (Extend_tyenv_with_module _ _ tyenv) var = apply_tyenv tyenv var
apply_tyenv (Extend_tyenv_with_type _ _ tyenv) var = apply_tyenv tyenv var

lookup_qualified_var_in_tyenv :: Identifier -> Identifier -> TyEnv -> Type
lookup_qualified_var_in_tyenv mod_var var tyenv =
  let iface = lookup_module_name_in_tyenv tyenv mod_var in 
    case iface of
      SimpleIface decls -> 
        lookup_variable_name_in_decls var decls

lookup_qualified_type_in_tyenv :: Identifier -> Identifier -> TyEnv -> Type
lookup_qualified_type_in_tyenv mod_var tvar tyenv =
  let iface = lookup_module_name_in_tyenv tyenv mod_var in 
    case iface of
      SimpleIface decls -> 
        lookup_variable_name_in_decls tvar decls  -- Exercise: Fix the case that tvar is a variable name.

lookup_type_name_in_tyenv :: Identifier -> TyEnv -> Type
lookup_type_name_in_tyenv tname Empty_tyenv = 
  error $ "lookup_type_name_in_tyenv: " ++ tname ++ " not found"
lookup_type_name_in_tyenv tname (Extend_tyenv_with_type tname1 ty tyenv)
  | tname == tname1 = ty
  | otherwise = lookup_type_name_in_tyenv tname tyenv
lookup_type_name_in_tyenv tname (Extend_tyenv _ _ tyenv) = 
  lookup_type_name_in_tyenv tname tyenv
lookup_type_name_in_tyenv tname (Extend_tyenv_with_module _ _ tyenv) =
  lookup_type_name_in_tyenv tname tyenv

lookup_module_name_in_tyenv :: TyEnv -> Identifier -> Interface
lookup_module_name_in_tyenv Empty_tyenv mod_var = 
  error $ "lookup_module_name_in_tyenv: " ++ mod_var ++ " not found"
lookup_module_name_in_tyenv (Extend_tyenv _ _ tyenv) mod_var = 
  lookup_module_name_in_tyenv tyenv mod_var
lookup_module_name_in_tyenv (Extend_tyenv_with_module m iface tyenv) mod_var
  | m == mod_var = iface 
  | otherwise = lookup_module_name_in_tyenv tyenv mod_var
lookup_module_name_in_tyenv (Extend_tyenv_with_type _ _ tyenv) mod_var =
  lookup_module_name_in_tyenv tyenv mod_var

lookup_variable_name_in_decls :: Identifier -> [Declaration] -> Type
lookup_variable_name_in_decls var [] = 
  error $ "lookup_variable_name_in_decls: " ++ var ++ " not found"
lookup_variable_name_in_decls var (ValDecl x ty : decls) 
  | var == x = ty
  | otherwise = lookup_variable_name_in_decls var decls
lookup_variable_name_in_decls var (OpaqueTypeDecl x : decls)
  | var == x = error $ "lookup_variable_name_in_decls: " ++ var ++ " is an opaque type"
  | otherwise = lookup_variable_name_in_decls var decls
lookup_variable_name_in_decls var (TransparentTypeDecl x ty : decls)
  | var == x = ty
  | otherwise = lookup_variable_name_in_decls var decls

