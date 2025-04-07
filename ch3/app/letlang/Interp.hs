module Interp where

import Expr
import Env

--
value_of :: Exp -> Env -> ExpVal

value_of (Const_Exp n) env = Num_Val n

value_of (Var_Exp var) env = apply_env env var

value_of (Diff_Exp exp1 exp2) env =
  let val1 = value_of exp1 env
      val2 = value_of exp2 env

      num1 = expval_num val1
      num2 = expval_num val2
  in  Num_Val (num1 - num2)
  
value_of (IsZero_Exp exp) env =
  let val1 = value_of exp env in
    let num1 = expval_num val1 in
      if num1 == 0
      then Bool_Val True
      else Bool_Val False

value_of (If_Exp exp1 exp2 exp3) env =
  let val1 = value_of exp1 env in
    if expval_bool val1
    then value_of exp2 env
    else value_of exp3 env

value_of (Let_Exp var exp1 body) env =
  let val1 = value_of exp1 env in
    value_of body (extend_env var val1 env)
    

--
value_of_program :: Exp -> ExpVal

value_of_program exp = value_of exp initEnv


--
initEnv :: Env
initEnv = extend_env "i" (Num_Val 1)
            (extend_env "v" (Num_Val 5)
              (extend_env "x" (Num_Val 10) empty_env))

          

