
-- The syntax is based on the implicitrefs language, and
-- the semantics is based on the one for the continuation-based language.

module Interp where

import Expr
import EnvStore

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
  | Set_Rhs_Cont Location Cont

apply_cont :: Store -> Cont -> ExpVal -> (FinalAnswer, Store)
apply_cont store End_Cont v = (v, store)

apply_cont store (Zero1_Cont cont) num1 =
  apply_cont store cont
    (if expval_num num1 == 0
     then Bool_Val True
     else Bool_Val False)
    
apply_cont store (Let_Exp_Cont var body env cont) val1 =
  let (loc,store') = newref store val1
  value_of_k body (extend_env var loc env) store' cont

apply_cont store (If_Test_Cont exp2 exp3 env cont) v =
  if expval_bool v
  then value_of_k exp2 env store cont
  else value_of_k exp3 env store cont
  
apply_cont store (Diff1_Cont exp2 env cont) val1 =
  value_of_k exp2 env store (Diff2_Cont val1 cont)

apply_cont store (Diff2_Cont val1 cont) val2 =
  let num1 = expval_num val1
      num2 = expval_num val2
  in  apply_cont store cont (Num_Val (num1 - num2))

apply_cont store (Unop_Arg_Cont op cont) val =
  apply_cont store cont (apply_unop op val)

apply_cont store (Rator_Cont rand env cont) ratorVal =
  value_of_k rand env store (Rand_Cont ratorVal cont)

apply_cont store (Rand_Cont ratorVal cont) randVal =
  let proc = expval_proc ratorVal in
    apply_procedure_k proc randVal store cont

apply_cont store (Try_Cont var handler_exp env cont) val =
  apply_cont store cont val
                           
apply_cont store (Raise1_Cont cont) val =
  apply_handler val store cont

apply_cont store (Set_Rhs_Cont loc cont) val =
  let store' = setref store loc val in
    apply_cont store' cont (Num_Val 23)


apply_handler :: ExpVal -> Store -> Cont -> (FinalAnswer, Store)

apply_handler val store (Try_Cont var handler_exp env saved_cont) =
  value_of_k handler_exp (extend_env var val env) store saved_cont

apply_handler val store (End_Cont) =
  error ("Uncaught exception: " ++ show val)

apply_handler val store (Zero1_Cont cont) = apply_handler val store cont

apply_handler val store (Let_Exp_Cont x body env cont) = apply_handler val store cont

apply_handler val store (If_Test_Cont exp2 exp3 env cont) = apply_handler val store cont

apply_handler val store (Diff1_Cont exp env cont) = apply_handler val store cont

apply_handler val store (Diff2_Cont val1 cont) = apply_handler val store cont

apply_handler val store (Unop_Arg_Cont op cont) = apply_handler val store cont

apply_handler val store (Rator_Cont exp env cont) = apply_handler val store cont

apply_handler val store (Rand_Cont val1 cont) = apply_handler val store cont


apply_unop :: UnaryOp -> ExpVal -> ExpVal

apply_unop IsZero (Num_Val num)
  | num==0    = Bool_Val True
  | otherwise = Bool_Val False
apply_unop IsNull (List_Val [])  = Bool_Val True
apply_unop IsNull (List_Val _)   = Bool_Val False
apply_unop Car (List_Val (x:_))  = x
apply_unop Cdr (List_Val (_:xs)) = List_Val xs

--
value_of_k :: Exp -> Env -> Store -> Cont -> (FinalAnswer, Store)

value_of_k (Const_Exp n) env store cont = apply_cont store cont (Num_Val n)

value_of_k (Const_List_Exp nums) env store cont = apply_cont store cont (List_Val (map Num_Val nums))

value_of_k (Var_Exp var) env store cont =
  let (loc,store') = apply_env env store var
      val = deref store' loc
  in
    apply_cont store cont val

value_of_k (Diff_Exp exp1 exp2) env store cont =
  value_of_k exp1 env store (Diff1_Cont exp2 env cont)

value_of_k (Unary_Exp op exp1) env store cont =
  value_of_k exp1 env store (Unop_Arg_Cont op cont)
  
value_of_k (If_Exp exp1 exp2 exp3) env store cont =
  value_of_k exp1 env store (If_Test_Cont exp2 exp3 env cont)

value_of_k (Let_Exp var exp1 body) env store cont =
  value_of_k exp1 env store (Let_Exp_Cont var body env cont)

value_of_k (Letrec_Exp proc_name bound_var proc_body letrec_body) env store cont =
  value_of_k letrec_body (extend_env_rec proc_name bound_var proc_body env) store cont

value_of_k (Proc_Exp var body) env store cont =
  apply_cont store cont (Proc_Val (procedure var body env))

value_of_k (Call_Exp rator rand) env store cont =
  value_of_k rator env store (Rator_Cont rand env cont)
  
value_of_k (Try_Exp exp var handler_exp) env store cont =
  value_of_k exp env store (Try_Cont var handler_exp env cont)

value_of_k (Raise_Exp exp) env store cont =
  value_of_k exp env store (Raise1_Cont cont)

value_of_k (Block_Exp []) env store cont =
  apply_cont store cont (Num_Val 23)

value_of_k (Block_Exp exps) env store cont = x

value_of_k (Set_Exp x exp) env store cont =
  let (loc,store') = apply_env env store x in
  value_of_k exp env store' (Set_Rhs_Cont loc cont)

--
value_of_program :: Exp -> ExpVal

value_of_program exp = fst $ value_of_k exp initEnv initStore End_Cont


--
initEnv = empty_env

--
apply_procedure_k :: Proc -> ExpVal -> Store -> Cont -> (FinalAnswer, Store)
apply_procedure_k proc arg store cont =
  let (loc,store') = newref store arg in
   value_of_k (body proc) (extend_env (var proc) loc (saved_env proc)) store' cont
