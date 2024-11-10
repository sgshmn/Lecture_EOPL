module TypeCheckerTest where

import Expr

typechecker_tests :: TypeDeclTestSuite
typechecker_tests =
  TypeDeclTestSuite
   [
     -- simple arithmetic
     
     TDTC "positive-const" "11" (Just TyInt),
     TDTC "negative-const" "-33" (Just TyInt),
     TDTC "simple-arith-1" "-(44,33)" (Just TyInt),
  
     -- nested arithmetic
     TDTC "nested-arith-left" "-(-(44,33),22)" (Just TyInt),
     TDTC "nested-arith-right" "-(55, -(22,11))" (Just TyInt),
  
     -- simple variables
     TDTC "test-var-1" "x" (Just TyInt),
     TDTC "test-var-2" "-(x,1)" (Just TyInt),
     TDTC "test-var-3" "-(1,x)" (Just TyInt),

     TDTC "zero-test-1" "zero?(-(3,2))" (Just TyBool),
     TDTC "zero-test-2" "-(2,zero?(0))" Nothing,
      
     -- simple unbound variables
     TDTC "test-unbound-var-1" "foo" Nothing,
     TDTC "test-unbound-var-2" "-(x,foo)" Nothing,
  
     -- simple conditionals
     TDTC "if-true" "if zero?(1) then 3 else 4" (Just TyInt),
     TDTC "if-false" "if zero?(0) then 3 else 4" (Just TyInt),

     -- make sure that the test and both arms get evaluated
     -- properly. 
     TDTC "if-eval-test-true" "if zero?(-(11,12)) then 3 else 4" (Just TyInt),
     TDTC "if-eval-test-false" "if zero?(-(11, 11)) then 3 else 4" (Just TyInt),
     TDTC "if-eval-then" "if zero?(1) then -(22,1) else -(22,2)" (Just TyInt),
     TDTC "if-eval-else" "if zero?(0) then -(22,1) else -(22,2)" (Just TyInt),
      
     -- make sure types of arms agree (new for lang5-1)
     TDTC "if-compare-arms" "if zero?(0) then 1 else zero?(1)" Nothing,
     TDTC "if-check-test-is-boolean" "if 1 then 11 else 12" Nothing,

     -- simple let
     TDTC "simple-let-1" "let x = 3 in x" (Just TyInt),

     -- make sure the body and rhs get evaluated
     TDTC "eval-let-body" "let x = 3 in -(x,1)" (Just TyInt),
     TDTC "eval-let-rhs" "let x = -(4,1) in -(x,1)" (Just TyInt),

     -- check nested let and shadowing
     TDTC "simple-nested-let" "let x = 3 in let y = 4 in -(x,y)" (Just TyInt),
     TDTC "check-shadowing-in-body" "let x = 3 in let x = 4 in x" (Just TyInt),
     TDTC "check-shadowing-in-rhs" "let x = 3 in let x = -(x,1) in x" (Just TyInt),

     -- simple applications
     TDTC "apply-proc-in-rator-pos" "(proc(x : int) -(x,1)  30)" (Just TyInt),
     TDTC "checker-doesnt-ignore-type-info-in-proc"
        "(proc(x : (int -> int)) -(x,1)  30)"
        Nothing,
      
     TDTC "apply-simple-proc" "let f = proc (x : int) -(x,1) in (f 30)" (Just TyInt),
     TDTC "let-to-proc-1" "(proc(f : (int -> int))(f 30)  proc(x : int)-(x,1))" (Just TyInt),


     TDTC "nested-procs" "((proc (x : int) proc (y : int) -(x,y)  5) 6)" (Just TyInt),
     TDTC "nested-procs2"
        "let f = proc (x : int) proc (y : int) -(x,y) in ((f -(10,5)) 3)"
        (Just TyInt),
      
     -- simple letrecs
     TDTC "simple-letrec-1" "letrec int f(x : int) = -(x,1) in (f 33)" (Just TyInt),
     TDTC "simple-letrec-2"
        "letrec int f(x : int) = if zero?(x) then 0 else -((f -(x,1)), -2) in (f 4)"
        (Just TyInt),

     TDTC "simple-letrec-3"
        "let m = -5 \
           \ in letrec int f(x : int) = if zero?(x) then -((f -(x,1)), m) else 0 in (f 4)"
        (Just TyInt),

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
            (Just (Tyfun TyInt TyInt)),

      
      -- multiple letrecs no longer in the language
      TDTC "pgm8b" " \
               \ letrec ? odd (x : ?) = if zero?(x) then 0 else (even -(x,1)) \
               \ in letrec ? even(x : ?) = if zero?(x) then 1 else (odd -(x,1))\
               \ in odd"
            (Just (Tyfun TyInt TyInt)),

      TDTC "pgm8b" " \
               \ letrec ? odd (x : ?) = if zero?(x) then 0 else (even -(x,1)) \
               \ in letrec ? even(x : bool) = if zero?(x) then 1 else (odd -(x,1))\
               \ in odd"
            Nothing,


      -- circular type
      -- TDTC "circular-type" " \
      --          \ let fix = proc (f : ?) \
      --                      \ let d = proc (x : ?) proc (z : ?) (f (x x) z) \
      --                      \ in proc (n : ?) (f (d d) n) \
      --          \ in let t4m = proc (f : ?, x : ?) \
      --                            \ if zero?(x) then 0 else -(4, -(0,(f -(x,1)))) \
      --          \ in let times4 = (fix t4m) \
      --          \ in (times4 3)"
      --       Nothing,

      -- https://github.com/mwand/eopl3/blob/master/chapter7/inferred/tests.scm
      -- 278번째 줄 proc (f : ?, x : ?) 이거를 어떻게 표현할지
      -- proc (f : ?) proc (x : ?)로 표현 할지 고민

      
      -- multiple arguments not in the language
      -- TDTC "pgm11b" " \
      --          \ letrec ? even (odd : ?, x : ?) = if zero?(x) then 1 else (odd -(x,1)) \
      --          \ in letrec ? odd(x : ?) = if zero?(x) then 0 else (even odd -(x,1))
      --          \ in (odd 13)"
      --       (Just TyInt),
      -- 여기도 비슷한 이유로 주석 처리
      -- letrec ? even (odd : ?, x : ?)

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
            (Just (TyFun (TyVar 1) (Tyvar 2))),

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
