module TypeCheckerTest where

import Expr(Type(..))
import Testcase

typechecker_tests :: TestSuite
typechecker_tests =
  TestSuite
   [
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

     TDTC "letrec-apply-fcn" " \
          \ let f = proc (x : int) proc (y : int) -(x,y)  \
          \ in letrec \
          \     int loop(x : int) = if zero?(x) then 1 else ((f x) (loop -(x,1))) \
          \   in (loop 4)"
        (Just TyInt),

      TDTC "modules-declare-and-ignore" "\
            \ module m \
            \     interface \
            \        [u : int] \
            \     body \
            \        [u = 3] \
            \ 33"
         (Just TyInt),

      TDTC "modules-take-one-value" "\
            \ module m \
            \     interface \
            \        [u : int] \
            \     body \
            \        [u = 3] \
            \ from m take u"
         (Just TyInt),

      -- ?? : same with modules-take-one-value
      TDTC "modules-take-one-value-no-import" "\
            \ module m \
            \     interface \
            \        [u : int] \
            \     body \
            \        [u = 3] \
            \ from m take u"
         (Just TyInt),

      -- Parse error
      TDTC "modules-take-from-parameterized-module" "\
            \ module m \
            \     interface \
            \        ((m1 : []) => [u : int]) \
            \     body \
            \        module-proc (m1 : []) [u = 3] \
            \ from m take u"
         Nothing,

      TDTC "modules-check-iface-subtyping-1" "\
            \ module m \
            \     interface \
            \        [u : int] \
            \     body \
            \        [u = 3 v = 4] \
            \ from m take u"
         (Just TyInt),

      -- if the interpreter always called the typechecker, or put
      -- only declared variables in the module, this would raise an
      -- error.  Exercise: make this modification.
      TDTC "modules-take-one-value-but-interface-bad" "\
            \ module m interface []  body [u = 3] \
            \ from m take u"
         Nothing,
      
      TDTC "modules-take-bad-value" "\
            \ module m interface []  body [u = 3] \
            \ from m take x"
         Nothing,

      TDTC "modules-two-vals" "\
            \ module m \
            \     interface \
            \        [u : int \
            \         v : int] \
            \     body \
            \        [u = 44 \
            \         v = 33] \
            \ -(from m take u, from m take v)"
         (Just TyInt),

      TDTC "modules-two-vals-bad-interface-1" "\
            \ module m interface [u : int v : bool]  \
            \           body [u = 44 v = 33] \
            \ -(from m take u, from m take v)"
         Nothing,

      TDTC "modules-extra-vals-are-ok-1" "\
            \ module m interface [x : int] body [x = 3 y = 4] \
            \ from m take x"
         (Just TyInt),

      TDTC "module-extra-vals-are-ok-2" "\
            \ module m interface [y : int] body [x = 3 y = 4] \
            \ from m take y"
         (Just TyInt),

      TDTC "modules-two-vals-bad-interface-14" "\
            \ module m interface\
            \        [v : int \
            \         u : int] \
            \     body \
            \        [v = zero?(0) u = 33] \
            \ -(from m take u, from m take v)"
         Nothing,

      TDTC "modules-check-let*-1" "\
            \ module m interface      [u : int v : int] \
            \          body [u = 44  v = -(u,11)] \
            \ -(from m take u, from m take v)"
         (Just TyInt),

      TDTC "modules-check-let*-2.0" "\
            \ module m1 interface [u : int] body [u = 44] \
            \ module m2 interface [v : int] \
            \           body \
            \              [v = -(from m1 take u,11)] \
            \ -(from m1 take u, from m2 take v)"
         (Just TyInt),

      TDTC "modules-check-let*-2.05" "\
            \ module m1 interface [u : int] body [u = 44] \
            \ module m2 interface [v : int] body [v = -(from m1 take u,11)] \
            \ 33"
         (Just TyInt),

      TDTC "modules-check-let*-2.1" "\
            \ module m1 interface [u : int] body [u = 44] \
            \ module m2  \
            \     interface [v : int] \
            \     body [v = -(from m1 take u,11)] \
            \ -(from m1 take u, from m2 take v)"
         (Just TyInt),

      TDTC "modules-check-let*-2.2" "\
            \ module m2 \
            \     interface [v : int]   \
            \     body  \
            \        [v = -(from m1 take u,11)] \
            \ module m1 interface [u : int] body [u = 44] \
            \ -(from m1 take u, from m2 take v)"
         Nothing
   ]
