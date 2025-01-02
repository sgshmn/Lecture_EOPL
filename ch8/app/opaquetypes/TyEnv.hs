{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module TyEnv where

import Expr(Identifier, Type, Interface(..), Declaration(..))
import EitherState

-- Invariant: Types in type environments are always fully expanded. 
data TyEnv = 
    Empty_tyenv
  | Extend_tyenv Identifier Type TyEnv
  | Extend_tyenv_with_module Identifier Interface TyEnv
  | Extend_tyenv_with_type Identifier Type TyEnv
  deriving Show

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

apply_tyenv :: TyEnv -> Identifier -> Either_ String Type 
apply_tyenv Empty_tyenv var = _Left $ "Variable not found: " ++ var
apply_tyenv (Extend_tyenv v ty tyenv) var
  | var == v = _Right ty
  | otherwise = apply_tyenv tyenv var
apply_tyenv (Extend_tyenv_with_module _ _ tyenv) var = apply_tyenv tyenv var
apply_tyenv (Extend_tyenv_with_type _ _ tyenv) var = apply_tyenv tyenv var

lookup_qualified_var_in_tyenv :: Identifier -> Identifier -> TyEnv -> Either_ String Type
lookup_qualified_var_in_tyenv mod_var var tyenv =
  do iface <- lookup_module_name_in_tyenv tyenv mod_var
     case iface of
       SimpleIface decls -> 
         lookup_variable_name_in_decls var decls

lookup_qualified_type_in_tyenv :: Identifier -> Identifier -> TyEnv -> Either_ String Type
lookup_qualified_type_in_tyenv mod_var tvar tyenv =
  do iface <- lookup_module_name_in_tyenv tyenv mod_var
     case iface of
       SimpleIface decls -> 
         lookup_variable_name_in_decls tvar decls  -- Exercise: Fix the case that tvar is a variable name.

lookup_type_name_in_tyenv :: Identifier -> TyEnv -> Either_ String Type
lookup_type_name_in_tyenv tname Empty_tyenv = 
  _Left $ "lookup_type_name_in_tyenv: " ++ tname ++ " not found"
lookup_type_name_in_tyenv tname (Extend_tyenv_with_type tname1 ty tyenv)
  | tname == tname1 = _Right ty
  | otherwise = lookup_type_name_in_tyenv tname tyenv
lookup_type_name_in_tyenv tname (Extend_tyenv _ _ tyenv) = 
  lookup_type_name_in_tyenv tname tyenv
lookup_type_name_in_tyenv tname (Extend_tyenv_with_module _ _ tyenv) =
  lookup_type_name_in_tyenv tname tyenv

lookup_module_name_in_tyenv :: TyEnv -> Identifier -> Either_ String Interface
lookup_module_name_in_tyenv Empty_tyenv mod_var = 
  _Left $ "lookup_module_name_in_tyenv: " ++ mod_var ++ " not found"
lookup_module_name_in_tyenv (Extend_tyenv _ _ tyenv) mod_var = 
  lookup_module_name_in_tyenv tyenv mod_var
lookup_module_name_in_tyenv (Extend_tyenv_with_module m iface tyenv) mod_var
  | m == mod_var = _Right iface 
  | otherwise = lookup_module_name_in_tyenv tyenv mod_var
lookup_module_name_in_tyenv (Extend_tyenv_with_type _ _ tyenv) mod_var =
  lookup_module_name_in_tyenv tyenv mod_var

lookup_variable_name_in_decls :: Identifier -> [Declaration] -> Either_ String Type
lookup_variable_name_in_decls var [] = 
  _Left $ "lookup_variable_name_in_decls: " ++ var ++ " not found"
lookup_variable_name_in_decls var (ValDecl x ty : decls) 
  | var == x = _Right ty
  | otherwise = lookup_variable_name_in_decls var decls
lookup_variable_name_in_decls var (OpaqueTypeDecl x : decls)
  | var == x = _Left $ "lookup_variable_name_in_decls: " ++ var ++ " is an opaque type"
  | otherwise = lookup_variable_name_in_decls var decls
lookup_variable_name_in_decls var (TransparentTypeDecl x ty : decls)
  | var == x = _Right ty
  | otherwise = lookup_variable_name_in_decls var decls

