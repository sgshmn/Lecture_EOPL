module TypeCheckerTest where

import Expr(Type(..))
import Testcase

typechecker_tests :: TestSuite
typechecker_tests =
  TestSuite
   [
     -- simple arithmetic
     TYCK "positive_const.let" (Just TyInt),
     TYCK "negative_const.let" (Just TyInt),
     TYCK "simple_arith_1.let" (Just TyInt),
  
     -- nested arithmetic
     TYCK "nested_arith_left.let" (Just TyInt),
     TYCK "nested_arith_right.let" (Just TyInt),
  
     -- simple variables
     TYCK "test_var_1.let" (Just TyInt),
     TYCK "test_var_2.let" (Just TyInt),
     TYCK "test_var_3.let" (Just TyInt),

     TDTC "zero-test-1" "zero?(-(3,2))" (Just TyBool),
     TDTC "zero-test-2" "-(2,zero?(0))" Nothing,
      
     -- simple unbound variables
     TYCK "test_unbound_var_1.let" Nothing,
     TYCK "test_unbound_var_2.let" Nothing,
  
     -- simple conditionals
     TYCK "if_true.let" (Just TyInt),
     TYCK "if_false.let" (Just TyInt),

     -- make sure that the test and both arms get evaluated
     -- properly. 
     TYCK "if_eval_test_true.let" (Just TyInt),
     TYCK "if_eval_test_false.let" (Just TyInt),
     TDTC "if-eval-then" "if zero?(1) then -(22,1) else -(22,2)" (Just TyInt),
     TDTC "if-eval-else" "if zero?(0) then -(22,1) else -(22,2)" (Just TyInt),
      
     -- make sure types of arms agree (new for lang5-1)
     TDTC "if-compare-arms" "if zero?(0) then 1 else zero?(1)" Nothing,
     TDTC "if-check-test-is-boolean" "if 1 then 11 else 12" Nothing,

     -- simple let
     TYCK "simple_let_1.let" (Just TyInt),

     -- make sure the body and rhs get evaluated
     TYCK "eval_let_body.let" (Just TyInt),
     TYCK "eval_let_rhs.let" (Just TyInt),

     -- check nested let and shadowing
     TYCK "simple_nested_let.let" (Just TyInt),
     TYCK "check_shadowing_in_body.let" (Just TyInt),
     TYCK "check_shadowing_in_rhs.let" (Just TyInt),

     -- simple applications
     TYCK "apply_proc_in_rator_pos.checked" (Just TyInt),
     TDTC "checker-doesnt-ignore-type-info-in-proc"
        "(proc(x : (int -> int)) -(x,1)  30)"
        Nothing,
      
     TYCK "apply_simple_proc.checked" (Just TyInt),
     TYCK "let_to_proc_1.checked" (Just TyInt),

     TYCK "nested_procs_1.checked" (Just TyInt),
     TYCK "nested_procs_2.checked" (Just TyInt),
      
     -- simple letrecs
     TYCK "simple_letrec_1.checked" (Just TyInt),
     TYCK "simple_letrec_2.checked" (Just TyInt),
     TYCK "simple_letrec_3.checked" (Just TyInt),

     TDTC "double-it" "letrec int double (n : int) = if zero?(n) then 0 \
                                \ else -( (double -(n,1)), -2) \
                                \ in (double 3)"
        (Just TyInt),

     -- tests of expressions that produce procedures
     TDTC "build-a-proc-typed" "proc (x : int) -(x,1)" (Just (TyFun TyInt TyInt)),

     TDTC "build-a-proc-typed-2" "proc (x : int) zero?(-(x,1))" (Just (TyFun TyInt TyBool)),
      
     TDTC "bind-a-proc-typed"
        "let f = proc (x : int) -(x,1) in (f 4)"
        (Just TyInt),

     TDTC "bind-a-proc-return-proc"
        "let f = proc (x : int) -(x,1) in f"
        (Just (TyFun TyInt TyInt)),

     TDTC "type-a-ho-proc-1"
        "proc(f : (int -> bool)) (f 3)"
        (Just (TyFun (TyFun TyInt TyBool) TyBool)),

     TDTC "type-a-ho-proc-2"
        "proc(f : (bool -> bool)) (f 3)"
        Nothing,

     TDTC "apply-a-ho-proc"
        "proc (x : int) proc (f : (int -> bool)) (f x)"
        (Just (TyFun TyInt (TyFun (TyFun TyInt TyBool) TyBool))),

     TDTC "apply-a-ho-proc-2"
        "proc (x : int) proc (f : (int -> (int -> bool))) (f x)"
        (Just (TyFun TyInt (TyFun (TyFun TyInt (TyFun TyInt TyBool)) (TyFun TyInt TyBool)))),

     TDTC "apply-a-ho-proc-3"
        "proc (x : int) proc (f : (int -> (int -> bool))) (f zero?(x))"
        Nothing,

     TDTC "apply-curried-proc"
        "((proc(x : int) proc (y : int)-(x,y)  4) 3)"
        (Just TyInt),

     TDTC "apply-a-proc-2-typed"
        "(proc (x : int) -(x,1) 4)" 
        (Just TyInt),

     TDTC "apply-a-letrec" " \
             \ letrec int f(x : int) = -(x,1) \
             \ in (f 40)"
                     (Just TyInt),

     TDTC "letrec-non-shadowing"
                "(proc (x : int) \
                   \ letrec bool loop(x : bool) =(loop x) \ 
                   \  in x \
                   \ 1)"
        (Just TyInt),

      
     TDTC "letrec-return-fact" " \
               \ let times = proc (x : int) proc (y : int) -(x,y) \   
               \ in letrec \
               \      int fact(x : int) = if zero?(x) then 1 else ((times x) (fact -(x,1))) \
               \    in fact"
        (Just (TyFun TyInt TyInt)),

     TDTC "letrec-apply-fact" " \
          \ let times = proc (x : int) proc (y : int) -(x,y) \
          \ in letrec \
          \     int fact(x : int) = if zero?(x) then 1 else ((times x) (fact -(x,1))) \
          \   in (fact 4)"
        (Just TyInt)

   ]
