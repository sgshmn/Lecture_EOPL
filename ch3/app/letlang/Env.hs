module Env where

import Expr

-- Environment
data Env =
    Empty_env
  | Extend_env Identifier ExpVal Env

empty_env :: Env
empty_env = Empty_env

apply_env :: Env -> Identifier -> ExpVal
apply_env Empty_env search_var = error (search_var ++ " is not found.")
apply_env (Extend_env saved_var saved_val saved_env) search_var
  | search_var==saved_var = saved_val
  | otherwise             = apply_env saved_env search_var

extend_env :: Identifier -> ExpVal -> Env -> Env
extend_env x v env = Extend_env x v env


