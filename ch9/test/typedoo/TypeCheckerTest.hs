{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
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
      TDTC "build-a-proc-typed" "proc (x : int) -(x,1)" (Just (TyFun [TyInt] TyInt)),

      TDTC "build-a-proc-typed-2" "proc (x : int) zero?(-(x,1))" (Just (TyFun [TyInt] TyBool)),
         
      TDTC "bind-a-proc-typed"
         "let f = proc (x : int) -(x,1) in (f 4)"
         (Just TyInt),

      TDTC "bind-a-proc-return-proc"
         "let f = proc (x : int) -(x,1) in f"
         (Just (TyFun [TyInt] TyInt)),

      TDTC "type-a-ho-proc-1"
         "proc(f : (int -> bool)) (f 3)"
         (Just (TyFun [TyFun [TyInt] TyBool] TyBool)),

      TDTC "type-a-ho-proc-2"
         "proc(f : (bool -> bool)) (f 3)"
         Nothing,

      TDTC "apply-a-ho-proc"
         "proc (x : int) proc (f : (int -> bool)) (f x)"
         (Just (TyFun [TyInt] (TyFun [TyFun [TyInt] TyBool] TyBool))),

      TDTC "apply-a-ho-proc-2"
         "proc (x : int) proc (f : (int -> (int -> bool))) (f x)"
         (Just (TyFun [TyInt] (TyFun [TyFun [TyInt] (TyFun [TyInt] TyBool)] (TyFun [TyInt] TyBool)))),

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
         (Just (TyFun [TyInt] TyInt)),

      TDTC "letrec-apply-fact" " \
            \ let times = proc (x : int) proc (y : int) -(x,y) \
            \ in letrec \
            \     int fact(x : int) = if zero?(x) then 1 else ((times x) (fact -(x,1))) \
            \   in (fact 4)"
         (Just TyInt),

         -- oop tests
         -- these should all check

      TDTC "test-self-1" " \
            \    class c extends object    \
            \             field int s   \
            \             method void initialize(v : int)set s = v   \
            \             method void sets(v : int)set s = v   \
            \             method int gets()s   \
            \             method void testit()send self sets(13)   \
            \                   \
            \    let o = new c (11)   \
            \          t1 = 0   \
            \          t2 = 0   \
            \       in begin    \
            \          set t1 = send o gets();   \
            \          send o testit();   \
            \          set t2 = send o gets();   \
            \          list(t1,t2)   \
            \          end"
               (Just (TyListOf TyInt)),

      TDTC "counter-1" " \
               \   class counter extends object   \
               \   field int count   \
               \      method void initialize()set count = 0   \
               \      method void countup()set count = +(count,1)   \
               \      method int getcount()count   \
               \         \
               \   let o1 = new counter ()   \
               \      t1 = 0   \
               \      t2 = 0   \
               \   in begin   \
               \      set t1 = send o1 getcount();   \
               \      send o1 countup();   \
               \      set t2 = send o1 getcount();   \
               \      list(t1,t2)   \
               \   end   \
               \   "       
               (Just (TyListOf TyInt)),

            -- 692
      TDTC "counter-1" " \
               \   class counter extends object   \
               \   field int count   \
               \      method void initialize()set count = 0   \
               \      method void countup()set count = +(count,1)   \
               \      method int getcount()count   \
               \         \
               \   let o1 = new counter ()   \
               \      t1 = 0   \
               \      t2 = 0   \
               \   in begin   \
               \      set t1 = send o1 getcount();   \
               \      send o1 countup();   \
               \      set t2 = send o1 getcount();   \
               \      list(t1,t2)   \
               \   end   \
               \   " 
               (Just (TyListOf TyInt)),
                  
      TDTC "shared-counter-1" " \
               \   class counter extends object   \
               \   field int count   \
               \      method void initialize()set count = 0   \
               \      method void countup()set count = +(count,1)   \
               \      method int getcount()count   \
               \         \
               \   class c1 extends object    \
               \      field int n   \
               \      field counter counter1   \
               \      method void initialize(a_counter : counter)   \
               \      begin   \
               \      set n = 0;   \
               \      set counter1 = a_counter   \
               \      end   \
               \      method void countup()   \
               \      begin   \
               \         send counter1 countup();   \
               \         set n = +(n,1)   \
               \      end   \
               \      method listof int getstate()list(n, send counter1 getcount())   \
               \         \
               \   let counter1 = new counter()   \
               \   in let o1 = new c1(counter1)   \
               \         o2 = new c1(counter1)   \
               \   in begin   \
               \      send o1 countup();   \
               \      send o2 countup();   \
               \      send o2 countup();   \
               \      list( send o1 getstate(),   \
               \            send o2 getstate())   \
               \      end" 
               (Just (TyListOf (TyListOf TyInt))),
                  
                  
      TDTC "inherit-1" " \
               \   class c1 extends object   \
               \   field int ivar1   \
               \   method void initialize()set ivar1 = 1   \
               \      \
               \   class c2 extends c1   \
               \   field int ivar2   \
               \   method void initialize()   \
               \   begin   \
               \      super initialize();   \
               \      set ivar2 = 1   \
               \   end   \
               \   method void setiv1(n : int)set ivar1 = n   \
               \   method int getiv1()ivar1   \
               \      \
               \   let o = new c2()   \
               \   t1 = 0   \
               \   in begin   \
               \      send o setiv1(33);   \
               \      send o getiv1()   \
               \   end" 
               (Just TyInt),
                           
               
      TDTC "inherit-3" " \
               \   class c1 extends object    \
               \   method int initialize()1   \
               \   method int m1()1   \
               \      \
               \   class c2 extends c1    \
               \   method int initialize()1    \
               \   method int m1()super m1()   \
               \   method int m2()2   \
               \      \
               \   class c3 extends c2    \
               \   method int initialize()1   \
               \   method int m1()3   \
               \   method int m2()super m2()   \
               \   method int m3()super m1()   \
               \      \
               \   let o = new c3 ()   \
               \   in list( send o m1(),   \
               \            send o m2(),   \
               \            send o m3()   \
               \         )"
               (Just (TyListOf TyInt)),

      TDTC "chris-1" " \  
               \   class aclass extends object   \ 
               \   field int i   \
               \   method void initialize(x : int) set i = x   \
               \   method int m(y : int) +(i,y)   \
               \      \
               \   let o1 = new aclass(3)   \
               \   in send o1 m(2)" 
               (Just TyInt),

                 
      TDTC "chris-2" " \
               \   class c1 extends object    \
               \   method int initialize() 1    \
               \   method int ma()1    \
               \   method int mb()send self ma()    \
               \       \
               \   class c2 extends c1   % just use c1's initialize    \
               \   method int ma() 2    \
               \       \
               \   let x = new c2 ()    \
               \   in list(send x ma(),send x mb())    \
               \   " 
               (Just (TyListOf TyInt)),

                
      TDTC "for-book-1" " \
               \   class c1 extends object    \
               \   field int i   \
               \   field int j   \
               \   method void initialize(x : int) begin set i = x; set j = -(0,x) end   \
               \   method void countup(d : int) begin set i = +(i,d); set j = -(j,d) end   \
               \   method listof int getstate()list(i,j)   \
               \      \
               \   let o1 = new c1(3)   \
               \      t1 = list(1)   \
               \      t2 = list(1)   \
               \   in begin   \
               \      set t1 = send o1 getstate();   \
               \      send o1 countup(2);   \
               \      set t2 = send o1 getstate();   \
               \      list(t1,t2)   \
               \      end" 
               (Just (TyListOf (TyListOf TyInt))),
      
      TDTC "odd-even-via-self" " \
               \   class oddeven extends object   \ 
               \   method int initialize()1   \
               \   method int even(n : int)if zero?(n) then 1 else send self odd(-(n,1))   \
               \   method int odd(n : int) if zero?(n) then 0 else send self even(-(n,1))   \
               \   \
               \   let o1 = new oddeven() in send o1 odd(13)" 
               (Just TyInt),
                   
      TDTC "for-book-2" " \
               \   class c1 extends object    \ 
               \   method int initialize()1   \ 
               \   method int m1()1   \ 
               \   method int m2()100   \ 
               \   method int m3()send self m2()   \ 
               \      \ 
               \   class c2 extends c1    \ 
               \   method int initialize()1   \ 
               \   method int m2()2   \ 
               \      \ 
               \   let o1 = new c1()   \ 
               \      o2 = new c2()   \ 
               \   in list(send o1 m1(),           % returns 1   \ 
               \         send o1 m2(),           % returns 100   \ 
               \         send o1 m3(),           % returns 100   \ 
               \         send o2 m1(),           % returns 1 (from c1)   \ 
               \         send o2 m2(),           % returns 2 (from c2)   \ 
               \         send o2 m3()            % returns 2 (c1's m3 calls c2's m2)   \ 
               \         ) " 
               (Just (TyListOf TyInt)),
                  
      TDTC "sum-leaves" " \
               \   class tree extends object    \
               \   method int initialize()1   \
               \      \
               \   class interior_node extends tree    \
               \   field tree left   \
               \   field tree right   \
               \   method void initialize(l : tree, r : tree)   \
               \      begin   \
               \      set left = l; set right = r   \
               \      end   \
               \   method int sum () +(send left sum(), send right sum())   \
               \      \
               \   class leaf_node extends tree    \
               \   field int value   \
               \   method void initialize(v : int)set value = v   \
               \   method int sum () value   \
               \      \
               \   let o1 = new interior_node (   \
               \            new interior_node (   \
               \               new leaf_node(3),   \
               \               new leaf_node(4)),   \
               \            new leaf_node(5))   \
               \   in send o1 sum()"
               Nothing,

                  
      TDTC "sum-leaves-1.5" " \
               \   class tree extends object    \
               \   method int initialize()1   \
               \   method int sum () 17   \
               \      \
               \   class interior_node extends tree    \
               \   field tree left   \
               \   field tree right   \
               \   method void initialize(l : tree, r : tree)   \
               \      begin   \
               \      set left = l; set right = r   \
               \      end   \
               \   method int sum () +(send left sum(), send right sum())   \
               \      \
               \   class leaf_node extends tree    \
               \   field int value   \
               \   method void initialize(v : int)set value = v   \
               \   method int sum () value   \
               \      \
               \   let o1 = new interior_node (   \
               \            new interior_node (   \
               \            new leaf_node(3),   \
               \            new leaf_node(4)),   \
               \            new leaf_node(5))   \
               \   in send o1 sum()"
               (Just TyInt),
                  
      TDTC "sum-leaves-2" " \
               \   interface tree   \
               \   method int sum (l : tree, r : tree)   \
               \      \
               \   class interior_node extends object   \
               \   field tree left   \
               \   field tree right   \
               \   method void initialize(l : tree, r : tree)   \
               \      begin   \
               \      set left = l; set right = r   \
               \      end   \
               \   method int sum() +(send left sum(), send right sum())   \
               \      \
               \   class leaf_node extends object   \
               \   field int value   \
               \   method void initialize(v : int)set value = v   \
               \   method int sum()value   \
               \      \
               \   let o1 = new interior_node (   \
               \            new interior_node (   \
               \            new leaf_node(3),   \
               \             new leaf_node(4)),   \
               \             new leaf_node(5))   \
               \   in send o1 sum()"
               Nothing,
                              
      TDTC "sum-leaves-with-abstract-method" " \
               \   interface tree   \
               \   method int sum()   \
               \      \
               \   class interior_node extends object    \
               \   implements tree   \
               \   field tree left   \
               \   field tree right   \
               \   method void initialize(l : tree, r : tree)   \
               \      begin   \
               \      set left = l; set right = r   \
               \      end   \
               \   method int sum()+(send left sum(), send right sum())   \
               \      \
               \   class leaf_node extends object   \
               \   implements tree    \
               \   field int value   \
               \   method void initialize(v : int)set value = v   \
               \   method int sum()value   \
               \      \
               \   let o1 = new interior_node (   \
               \            new interior_node (   \
               \               new leaf_node(3),   %% need subtyping to make this ok.   \
               \               new leaf_node(4)),   \
               \            new leaf_node(5))   \
               \   in send o1 sum()   \
               \   " 
               (Just TyInt),
                  
                  
      TDTC "equal-trees-1" " \
               \   interface tree   \
               \   method int sum()   \
               \   method bool equal(t : tree)   \
               \      \
               \   class interior_node extends object    \
               \   implements tree   \
               \   field tree left   \
               \   field tree right   \
               \   method void initialize(l : tree, r : tree)   \
               \      begin   \
               \      set left = l; set right = r   \
               \      end   \
               \   method tree getleft()left   \
               \   method tree getright()right   \
               \   method int sum()+(send left sum(), send right sum())   \
               \   method bool equal(t : tree)    \
               \      if instanceof t interior_node   \
               \      then if send left equal(send cast t interior_node getleft())   \
               \            then send right equal(send cast t interior_node getright())   \
               \            else zero?(1)   \
               \      else zero?(1)   \
               \         \
               \      \
               \   class leaf_node extends object    \
               \   implements tree   \
               \   field int value   \
               \   method void initialize(v : int)set value = v   \
               \   method int sum()value   \
               \   method int getvalue()value   \
               \   method bool equal(t : tree)   \
               \      if instanceof t leaf_node   \
               \      then zero?(-(value, send cast t leaf_node getvalue()))   \
               \      else zero?(1)   \
               \         \
               \      \
               \   let o1 = new interior_node (   \
               \            new interior_node (   \
               \               new leaf_node(3),      \
               \               new leaf_node(4)),   \
               \            new leaf_node(5))   \
               \   in send o1 equal(o1)   \
               \   " 
               (Just TyBool),
                  
                  
      TDTC "good-instanceof-1" " \
               \   class c1 extends object    \
               \   method int initialize () 1   \
               \   class c2 extends object    \
               \   method int initialize () 2   \
               \   let p = proc (o : c1) instanceof o c2 in 11   \
               \   " 
               (Just TyInt),
                  
      TDTC "up-cast-1" " \
               \   class c1 extends object    \
               \   method int initialize ()1   \
               \   method int get()2   \
               \      \
               \   class c2 extends c1    \
               \   let f = proc (o : c2) send cast o c1 get() in (f new c2())   \
               \   " 
               (Just TyInt),

                  
      TDTC "up-instance-1" " \
               \   class c1 extends object    \
               \   method int initialize ()1   \
               \   method int get()2   \
               \      \
               \   class c2 extends c1    \
               \   let f = proc (o : c2) instanceof o c1 in (f new c2())   \
               \   " 
               (Just TyBool),
                  
      TDTC "duplicate-methods-1 " " \
               \   class c1 extends object   \
               \   method int initialize() 1   \
               \   class c2 extends c1   \
               \   method int m1() 1   \
               \   method int m1() 2   \
               \   33" 
               Nothing,
                  
      TDTC "incomparable-instanceof-2"  " \
               \   class c1 extends object    \
               \   method int initialize ()1   \
               \   method int get()2   \
               \      \
               \   class c2 extends object    \
               \   method int initialize () 100   \
               \         \
               \   let f = proc (o : c2) if instanceof o c1 then 1 else 2 in (f new c2())   \
               \   " 
               (Just TyInt),
                  
      TDTC "equal-trees-by-double-dispatch" " \
               \   interface tree   \
               \   method int sum()   \
               \   method bool equal(t : tree)   \
               \   method bool equal_int(l : tree, r : tree)   \
               \   method bool equal_leaf(val : int)   \
               \      \
               \   class interior_node extends object   \
               \   implements tree    \
               \   field tree left   \
               \   field tree right   \
               \   method void initialize(l : tree, r : tree)   \
               \      begin   \
               \      set left = l; set right = r   \
               \      end   \
               \   method int sum() +(send left sum(), send right sum())   \
               \   method bool equal(t : tree) send t equal_int(left, right)   \
               \   method bool equal_int(l : tree, r : tree)   \
               \      if send left equal(l)   \
               \      then send right equal(r)   \
               \      else zero?(1)  % false   \
               \         \
               \   method bool equal_leaf(v : int) zero?(1)   \
               \      \
               \   class leaf_node extends object    \
               \   implements tree   \
               \   field int value   \
               \   field bool false   \
               \   method void initialize(v : int) begin set value = v; set   \
               \                                          false=zero?(1) end   \
               \   method int sum()value   \
               \   method bool equal(t : tree) send t equal_leaf(value)   \
               \   method bool equal_int(l : tree, r : tree) false   \
               \   method bool equal_leaf(otherval : int) zero?(-(value, otherval))   \
               \      \
               \   let o1 = new interior_node (   \
               \            new interior_node (   \
               \               new leaf_node(3),      \
               \               new leaf_node(4)),   \
               \            new leaf_node(5))   \
               \   in send o1 equal(o1)   \
               \   " 
               (Just TyBool),
                  
      TDTC "goldberg-80" " \
               \   class c1 extends object    \
               \   method int initialize () 1   \
               \   method int test () 1   \
               \   method int result1 () send self test ()   \
               \      \
               \   class c2 extends c1    \
               \   method int test () 2   \
               \      \
               \   class c3 extends c2    \
               \   method int result2 () send self result1 ()   \
               \   method int result3 () super test ()   \
               \      \
               \   class c4 extends c3    \
               \   method int test () 4   \
               \      \
               \   let o3 = new c3 ()   \
               \      o4 = new c4 ()   \
               \   in list(send o3 test(),   \
               \         send o4 result1 (),   \
               \         send o3 result2 (),   \   
               \         send o4 result2 (),   \
               \         send o3 result3 (),   \
               \         send o4 result3 ())   \
               \   "
               (Just (TyListOf TyInt)),
                  
      TDTC "check-interface-implementation-1" " \
               \   interface i1    \
               \   method int foo ()   \
               \      \
               \   class c1 extends object   \
               \   implements i1   \
               \   methid int initialize () 1   \
               \   method int bar () 27   \
               \   \
               \   13"
               Nothing,
                  
      TDTC "check-interface-implementation-2" " \
               \   interface i1    \
               \   method int foo ()   \
               \      \
               \   class c1 extends object   \
               \   implements i1   \
               \   method int initialize () 1   \
               \   method bool foo () 27   \
               \   \
               \   13"
               Nothing,
                  
               -- with exercise 9.34, this should become an error
                  
      TDTC "bad-cast-1" " \
               \   class c1 extends object   \
               \   method int initialize () 1   \
               \   class c2 extends object    \
               \   method int initialize () 2   \
               \   proc (o : c1) cast o c2   \
               \   "
               (Just (TyFun [TyClass "c1"] (TyClass "c2"))),
                  
      TDTC "missing-initialize-method-1" " \
               \   class c1 extends object    \
               \   method int initialize ()1   \
               \   method int get()2   \
               \      \
               \   class c2 extends object   % no initialize method!   \
               \   let f = proc (o : c2) instanceof o c1 in (f new c2())   \
               \   "
               Nothing,
                  
      TDTC "duplicate-methods-1" " \
               \   class c1 extends object   \
               \   method int initialize() 1   \
               \   class c2 extends c1   \
               \   method int m1() 1   \
               \   method int m1() 2   \
               \   33"
               Nothing,
                  
      TDTC "incomparable-instanceof-2" " \
               \   class c1 extends object    \
               \   method int initialize ()1   \
               \   method int get()2   \
               \      \
               \   class c2 extends object    \
               \   method int initialize () 100   \
               \         \
               \   let f = proc (o : c2) if instanceof o c1 then 1 else 2 in (f new c2())   \
               \   "
                  --      ;; this is stupid but legal
                  --      ;; exercise: make this illegal (9.34)
               (Just TyInt),
                  
      TDTC "bad-super-1" " \
               \   class c1 extends object    \
               \   method int initialize() 1   \
               \      \
               \   class c2 extends c1    \
               \   method int m1() super m2()   \
               \      \
               \   class c3 extends c2    \
               \   method int m2() 2   \
               \      \
               \   class c4 extends c3    \
               \   let o = new c4() in send o m1()   \
               \   "
               Nothing,
                  
      TDTC "unsupplied-method-2" " \
               \   interface c1    \
               \   method int m1()    \
               \      \
               \   class c2 extends object implements c1    \
               \   method int initialize () 0   \
               \   method int m2 ()send self m1()   \
               \   \
               \   33"
               Nothing,
                  
      TDTC "overriding-method-changes-type-1" " \
               \   class c1 extends object    \
               \   method int initialize () 1   \
               \   method int m1() 22   \
               \      \
               \   class c2 extends c1    \
               \   method bool m1() zero?(0)   \
               \  \ 
               \   33"
               Nothing,
                  
      TDTC "test6-3-1" " \
               \   class c1 extends object   \
               \   method int initialize () 1    \
               \   method int m1 () 11   \
               \   method int m2 () 12   \
               \   class c2 extends c1    \
               \   method int m1 () 21   \
               \   method int m2 () 22   \
               \   method int m3 () 23   \
               \   class c3 extends c2    \
               \   method int m4 () 34   \
               \   class c4 extends c3    \
               \   method int m2 () 42    \
               \   proc (o : c3) send o m2()   \
               \   "
               (Just (TyFun [TyClass "c3"] TyInt)),
                  
               --     ;; here c2 is bad, so the interprter runs successfully and returns
               --      ;; false. 
      TDTC "bad-instance-of-1" " \
               \   class c1 extends object   \
               \   method int initialize () 1   \
               \   \
               \   instanceof new c1() c2"
               (Just TyBool),
                  
               --      ;; here c1 is unrelated to c2, so the interpreter runs
               --      ;; successfully and returns false.
      TDTC "bad-instance-of-2" " \
               \   class c1 extends object  \
               \   method int initialize () 1  \
               \     \
               \   interface c2  \
               \  \   
               \   instanceof new c1() c2"
               (Just TyBool)
   ]
