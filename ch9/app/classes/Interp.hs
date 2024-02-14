module Interp where

import Expr
import EnvStore

--
value_of :: Exp -> Env -> Store -> (ExpVal, Store)   -- Sec 4.2.1: Store-passing specifications

value_of (Const_Exp n) env store = (Num_Val n,store)

value_of (Var_Exp var) env store = 
  let (loc,store1) = apply_env env store var 
      val = deref store1 loc
  in  (val, store1)

value_of (Diff_Exp exp1 exp2) env store =
  let (val1,store1) = value_of exp1 env store
      (val2,store2) = value_of exp2 env store1

      num1 = expval_num val1
      num2 = expval_num val2
  in  (Num_Val (num1 - num2), store2)
  
value_of (IsZero_Exp exp) env store =
  let (val1,store1) = value_of exp env store in
    let num1 = expval_num val1 in
      if num1 == 0
      then (Bool_Val True,store1)
      else (Bool_Val False,store1)

value_of (If_Exp exp1 exp2 exp3) env store =
  let (val1,store1) = value_of exp1 env store in
    if expval_bool val1
    then value_of exp2 env store1
    else value_of exp3 env store1

value_of (Let_Exp varExpList body) env store = undefined
  -- let (val1,store1) = value_of exp1 env store 
  --     (loc,store2)  = newref store1 val1
  -- in
  --   value_of body (extend_env var loc env) store2

value_of (Letrec_Exp letbindings letrec_body) env store =
  value_of letrec_body (extend_env_rec (extend letbindings) env) store
  where extend [] = []
        extend ((proc_name, bound_vars, proc_body):letbindings) =
          (proc_name,bound_vars,proc_body) : extend letbindings

value_of (Proc_Exp var body) env store =
  (Proc_Val (procedure var body env),store)

value_of (Call_Exp rator rands) env store =
  let (val1,store1) = value_of rator env store
      (val2,store2) = undefined -- value_of rand env store1
      
      proc = expval_proc val1
      arg  = val2
  in undefined -- apply_procedure proc arg store2

value_of (Block_Exp [exp]) env store = value_of exp env store

value_of (Block_Exp (exp:expList)) env store =
  let (_,store1) = value_of exp env store
  in value_of (Block_Exp expList) env store1

value_of (Set_Exp x exp) env store =
  let (val1,store1) = value_of exp env store
      (loc,store2) = apply_env env store x
  in  (Num_Val 23,setref store2 loc val1)       -- The dummy value, 23, comes from the EOPL book. :)

--
value_of_program :: Exp -> ExpVal

value_of_program exp =
  let (v,_) = value_of exp initEnv initStore in v


--
initEnv = empty_env

--
apply_procedure :: Proc -> ExpVal -> Store -> (ExpVal,Store)
apply_procedure proc arg store = undefined
--  let (loc,store1) = newref store arg in
--   value_of (body proc) (extend_env (vars proc) loc (saved_env proc)) store1
