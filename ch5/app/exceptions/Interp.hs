module Interp where

import Expr
import Env

type FinalAnswer = ExpVal 

-- Continuation

data Cont =
    End_Cont
  | Zero1_Cont Cont
  | Let_Exp_Cont Identifier Exp Env Cont
  | If_Test_Cont Exp Exp Env Cont
  | Diff1_Cont Exp Env Cont
  | Diff2_Cont ExpVal Cont
  | Rator_Cont Exp Env Cont
  | Rand_Cont ExpVal Cont
  | Unop_Arg_Cont UnaryOp Cont
  | Try_Cont Identifier Exp Env Cont
  | Raise1_Cont Cont

apply_cont :: Cont -> ExpVal -> FinalAnswer
apply_cont End_Cont v = v

apply_cont (Zero1_Cont cont) num1 =
  apply_cont cont
    (if expval_num num1 == 0
     then Bool_Val True
     else Bool_Val False)
    
apply_cont (Let_Exp_Cont var body env cont) val1 =
  value_of_k body (extend_env var val1 env) cont

apply_cont (If_Test_Cont exp2 exp3 env cont) v =
  if expval_bool v
  then value_of_k exp2 env cont
  else value_of_k exp3 env cont
  
apply_cont (Diff1_Cont exp2 env cont) val1 =
  value_of_k exp2 env (Diff2_Cont val1 cont)

apply_cont (Diff2_Cont val1 cont) val2 =
  let num1 = expval_num val1
      num2 = expval_num val2
  in  apply_cont cont (Num_Val (num1 - num2))

apply_cont (Unop_Arg_Cont op cont) val =
  apply_cont cont (apply_unop op val)

apply_cont (Rator_Cont rand env cont) ratorVal =
  value_of_k rand env (Rand_Cont ratorVal cont)

apply_cont (Rand_Cont ratorVal cont) randVal =
  let proc = expval_proc ratorVal in
    apply_procedure_k proc randVal cont

apply_cont (Try_Cont var handler_exp env cont) val =
  apply_cont cont val
                           
apply_cont (Raise1_Cont cont) val =
  apply_handler val cont



apply_handler :: ExpVal -> Cont -> FinalAnswer
apply_handler val (Try_Cont var handler_exp env saved_cont) =
  value_of_k handler_exp (extend_env var val env) saved_cont

apply_handler val (End_Cont) =
  error ("Uncaught exception: " ++ show val)

apply_handler val (Zero1_Cont cont) = apply_handler val cont

apply_handler val (Let_Exp_Cont x body env cont) = apply_handler val cont

apply_handler val (If_Test_Cont exp2 exp3 env cont) = apply_handler val cont

apply_handler val (Diff1_Cont exp env cont) = apply_handler val cont

apply_handler val (Diff2_Cont val1 cont) = apply_handler val cont

apply_handler val (Unop_Arg_Cont op cont) = apply_handler val cont

apply_handler val (Rator_Cont exp env cont) = apply_handler val cont

apply_handler val (Rand_Cont val1 cont) = apply_handler val cont


apply_unop :: UnaryOp -> ExpVal -> ExpVal 
apply_unop IsZero (Num_Val num)
  | num==0    = Bool_Val True
  | otherwise = Bool_Val False
apply_unop IsNull (List_Val [])  = Bool_Val True
apply_unop IsNull (List_Val _)   = Bool_Val False
apply_unop Car (List_Val (x:_))  = x
apply_unop Cdr (List_Val (_:xs)) = List_Val xs

--
value_of_k :: Exp -> Env -> Cont -> FinalAnswer

value_of_k (Const_Exp n) env cont = apply_cont cont (Num_Val n)

value_of_k (Const_List_Exp nums) env cont = apply_cont cont (List_Val (map Num_Val nums))

value_of_k (Var_Exp var) env cont = apply_cont cont (apply_env env var)

value_of_k (Diff_Exp exp1 exp2) env cont =
  value_of_k exp1 env (Diff1_Cont exp2 env cont)

value_of_k (Unary_Exp op exp1) env cont =
  value_of_k exp1 env (Unop_Arg_Cont op cont)
  
value_of_k (If_Exp exp1 exp2 exp3) env cont =
  value_of_k exp1 env (If_Test_Cont exp2 exp3 env cont)

value_of_k (Let_Exp var exp1 body) env cont =
  value_of_k exp1 env (Let_Exp_Cont var body env cont)

value_of_k (Letrec_Exp proc_name bound_var proc_body letrec_body) env cont =
  value_of_k letrec_body (extend_env_rec proc_name bound_var proc_body env) cont

value_of_k (Proc_Exp var body) env cont =
  apply_cont cont (Proc_Val (procedure var body env))

value_of_k (Call_Exp rator rand) env cont =
  value_of_k rator env (Rator_Cont rand env cont)
  
value_of_k (Try_Exp exp var handler_exp) env cont =
  value_of_k exp env (Try_Cont var handler_exp env cont)

value_of_k (Raise_Exp exp) env cont =
  value_of_k exp env (Raise1_Cont cont)

--
value_of_program :: Exp -> ExpVal

value_of_program exp = value_of_k exp initEnv End_Cont


--
initEnv = extend_env "i" (Num_Val 1)
            (extend_env "v" (Num_Val 5)
              (extend_env "x" (Num_Val 10) empty_env))

--
apply_procedure_k :: Proc -> ExpVal -> Cont -> FinalAnswer
apply_procedure_k proc arg cont =
   value_of_k (body proc) (extend_env (var proc) arg (saved_env proc)) cont
