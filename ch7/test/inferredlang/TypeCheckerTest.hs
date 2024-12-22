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
        (Just TyInt),

      TDTC "pgm7b" " \
               \ letrec ? fact (x : ?) = if zero?(x) then 1 else -(x, (fact -(x,1))) \
               \ in fact"
            (Just (TyFun TyInt TyInt)),

      
      -- multiple letrecs no longer in the language
      -- TDTC "pgm8b" " \
      --          \ letrec ? odd (x : ?) = if zero?(x) then 0 else (even -(x,1)) \
      --          \ in letrec ? even(x : ?) = if zero?(x) then 1 else (odd -(x,1))\
      --          \ in odd"
      --       Nothing,

      -- TDTC "pgm8ab" " \
      --          \ letrec ? odd (x : ?) = if zero?(x) then 0 else (even -(x,1)) \
      --          \ in letrec ? even(x : bool) = if zero?(x) then 1 else (odd -(x,1))\
      --          \ in odd"
      --       Nothing,


      -- circular type
      TDTC "circular-type" " \
               \ let fix = proc (f : ?) \
                           \ let d = proc (x : ?) proc (z : ?) ((f (x x)) z) \
                           \ in proc (n : ?) ((f (d d)) n) \
               \ in let t4m = proc (f : ?) proc(x : ?) \
                                 \ if zero?(x) then 0 else -(4, -(0,(f -(x,1)))) \
               \ in let times4 = (fix t4m) \
               \ in (times4 3)"
            Nothing,

      
      -- multiple arguments not in the language
      -- TDTC "pgm11b" " \
      --          \ letrec ? even (odd : ?, x : ?) = if zero?(x) then 1 else (odd -(x,1)) \
      --          \ in letrec ? odd(x : ?) = if zero?(x) then 0 else (even odd -(x,1))
      --          \ in (odd 13)"
      --       (Just TyInt),

      -- letrec ? even (odd : ?, x : ?) ?????

      TDTC "pgm11b-curried" " \
               \ letrec ? even (odd : ?) = proc (x : ?) if zero?(x) then 1 else (odd -(x,1)) \
               \ in letrec ? odd(x : ?) = if zero?(x) then 0 else ((even odd) -(x,1)) \
               \ in (odd 13)"
            (Just TyInt),

      TDTC "dont-infer-circular-type" " \
               \ letrec ? f (x : ?) = (f f) in 33"
            Nothing,

      TDTC "polymorphic-type-1" " \
               \ letrec ? f (x : ?) = (f x) in f"
            (Just (TyFun (TyVar 1) (TyVar 2))),

      -- this test should fail, because the type given is insufficiently
      -- polymorphic.  So we use it for testing the test harness, but not for
      -- testing the checker.

      -- TDTC "polymorphic-type-1a" " \
      --          \ letrec ? f (x : ?) = (f x) in f"
      --       (Just (TyFun (TyVar 1) (TyVar 1))),

      TDTC "polymorphic-type-2" " \
               \ letrec ? f (x : ?) = (f x) in proc (n : ?) (f -(n,1))"
            (Just (TyFun TyInt (TyVar 1)))
   ]
