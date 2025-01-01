{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module TypeCheckerTest where

import Expr(Type(..))
import Testcase

typechecker_tests :: TestSuite
typechecker_tests =
  TestSuite
   [
     -- [ tests for run ]
     -- simple arithmetic
     RUN "positive_const.let" (Just "11"),
     RUN "negative_const.let" (Just "-33"),
     RUN "simple_arith_1.let" (Just "11"),
  
     -- nested arithmetic
     RUN "nested_arith_left.let" (Just "-11"),
     RUN "nested_arith_right.let" (Just "44"),
  
     -- simple variables
     RUN "test_var_1.let" (Just "10"),
     RUN "test_var_2.let" (Just "9"),
     RUN "test_var_3.let" (Just "-9"),

     -- simple unbound variables
     RUN "test_unbound_var_1.let" Nothing,
     RUN "test_unbound_var_2.let" Nothing,
  
     -- simple conditionals
     RUN "if_true.let" (Just "3"),
     RUN "if_false.let" (Just "4"),

     -- test dynamic typechecking
     RUN "no_bool_to_diff_1.let" Nothing,
     RUN "no_bool_to_diff_2.let" Nothing,
     RUN "no_int_to_if.let" Nothing,

     -- make sure that the test and both arms get evaluated
     -- properly. 
     RUN "if_eval_test_true.let" (Just "3"),
     RUN "if_eval_test_false.let" (Just "4"),

     -- and make sure the other arm doesn't get evaluated.
     RUN "if_eval_test_true_2.let" (Just "3"),
     RUN "if_eval_test_false_2.let" (Just "4"),

     -- simple let
     RUN "simple_let_1.let" (Just "3"),

     -- make sure the body and rhs get evaluated
     RUN "eval_let_body.let" (Just "2"),
     RUN "eval_let_rhs.let" (Just "2"),

     -- check nested let and shadowing
     RUN "simple_nested_let.let" (Just "-1"),
     RUN "check_shadowing_in_body.let" (Just "4"),
     RUN "check_shadowing_in_rhs.let" (Just "2"),

     -- simple applications
     RUN "apply_proc_in_rator_pos.checked" (Just "29"),
     RUN "interp_ignores_type_info_in_proc.checked" (Just "29"),
     RUN "apply_simple_proc.checked" (Just "29"),
     RUN "let_to_proc_1.checked" (Just "29"),

     RUN "nested_procs_1.checked" (Just "-1"),
     RUN "nested_procs_2.checked" (Just "-1"),

     RUN "y_combinator_1.checked" (Just "12"),

     -- simple letrecs
     RUN "simple_letrec_1.checked" (Just "32"),
     RUN "simple_letrec_2.checked" (Just "8"),
     RUN "simple_letrec_3.checked" (Just "20"),

     -- typed oop
     RUN "test_self_1.typedoo" (Just "(11 13)"),
     RUN "counter_1.typedoo" (Just "(0 1)"),
     RUN "shared_counter_1.typedoo" (Just "((1 3) (2 3))"),
     RUN "inherit_1.typedoo" (Just "33"),
     RUN "inherit_3.typedoo" (Just "(3 2 1)"),
     RUN "chris_1.typedoo" (Just "5"),
     RUN "chris_2.typedoo" (Just "(2 2)"),
     RUN "for_book_1.typedoo" (Just "((3 -3) (5 -5))"),
     RUN "odd_even_via_self.typedoo" (Just "1"),
     RUN "for_book_2.typedoo" (Just "(1 100 100 1 2 2)"),
     RUN "sum_leaves.typedoo" (Just "12"),
     RUN "sum_leaves_2.typedoo" (Just "12"),
     RUN "sum_leaves_with_abstract_method.typedoo" (Just "12"),
     RUN "equal_trees_1.typedoo" (Just "True"),
     RUN "up_cast_1.typedoo" (Just "2"),
     RUN "up_instance_1.typedoo" (Just "True"),
     RUN "duplicate_methods_1.typedoo" (Just "33"),
     RUN "incomparable_instanceof_2.typedoo" (Just "2"),
     RUN "equal_trees_by_double_dispatch.typedoo" (Just "True"),
     RUN "goldberg_80.typedoo" (Just "(2 4 2 4 2 2)"),
    
    --  [ tests for check ]
    --  simple arithmetic
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

     TYCK "zero_test_1.let" (Just TyBool),
     TYCK "zero_test_2.let" Nothing,

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
     TYCK "if_eval_then.let" (Just TyInt),
     TYCK "if_eval_else.let" (Just TyInt),

    -- make sure types of arms agree (new for lang5-1)
     TYCK "if_compare_arms.let" Nothing,
     TYCK "if_check_test_is_boolean.let" Nothing,

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
     TYCK "interp_ignores_type_info_in_proc.checked" Nothing,
     TYCK "apply_simple_proc.checked" (Just TyInt),
     TYCK "let_to_proc_1.checked" (Just TyInt),

     TYCK "nested_procs_1.checked" (Just TyInt),
     TYCK "nested_procs_2.checked" (Just TyInt),

    -- simple letrecs
     TYCK "simple_letrec_1.checked" (Just TyInt),
     TYCK "simple_letrec_2.checked" (Just TyInt),
     TYCK "simple_letrec_3.checked" (Just TyInt),

     TYCK "double_it.checked" (Just TyInt),

    -- tests of expressions that produce procedures
     TYCK "build_a_proc_typed_1.checked" (Just (TyFun [TyInt] TyInt)),
     TYCK "build_a_proc_typed_2.checked" (Just (TyFun [TyInt] TyBool)),
     TYCK "bind_a_proc_typed.checked" (Just TyInt),
     TYCK "bind_a_proc_return_proc.checked" (Just (TyFun [TyInt] TyInt)),

     TYCK "type_a_ho_proc_1.checked" (Just (TyFun [(TyFun [TyInt] TyBool)] TyBool)),
     TYCK "type_a_ho_proc_2.checked" Nothing,

     TYCK "apply_a_ho_proc_1.checked" (Just (TyFun [TyInt] (TyFun [(TyFun [TyInt] TyBool)] TyBool))),
     TYCK "apply_a_ho_proc_2.checked" (Just (TyFun [TyInt] (TyFun [(TyFun [TyInt] (TyFun [TyInt] TyBool))] (TyFun [TyInt] TyBool)))),
     TYCK "apply_a_ho_proc_3.checked" Nothing,

     TYCK "apply_curried_proc.checked" (Just TyInt),
     TYCK "apply_a_proc_2_typed.checked" (Just TyInt),
     TYCK "apply_a_letrec.checked" (Just TyInt),

     TYCK "letrec_non_shadowing.checked" (Just TyInt),
     TYCK "letrec_return_fact.checked" (Just (TyFun [TyInt] TyInt)),
     TYCK "letrec_apply_fact.checked" (Just TyInt),

    -- oop tests
    -- these should all check.
     TYCK "test_self_1.typedoo" (Just (TyListOf TyInt)),
     TYCK "counter_1.typedoo" (Just (TyListOf TyInt)),
     TYCK "shared_counter_1.typedoo" (Just (TyListOf (TyListOf TyInt))),

     TYCK "inherit_1.typedoo" (Just TyInt),
     TYCK "inherit_3.typedoo" (Just (TyListOf TyInt)),
     TYCK "chris_1.typedoo" (Just TyInt),
     TYCK "chris_2.typedoo" (Just (TyListOf TyInt)),

     TYCK "for_book_1.typedoo" (Just (TyListOf (TyListOf TyInt))),
     TYCK "odd_even_via_self.typedoo" (Just TyInt),
     TYCK "for_book_2.typedoo" (Just (TyListOf TyInt)),
     
     TYCK "sum_leaves.typedoo" Nothing,
     TYCK "sum_leaves_1_5.typedoo" (Just TyInt),
     TYCK "sum_leaves_2.typedoo" Nothing,
     TYCK "sum_leaves_with_abstract_method.typedoo" (Just TyInt),
     
     TYCK "equal_trees_1.typedoo" (Just TyBool),
     TYCK "good_instanceof_1.typedoo" (Just TyInt),
     TYCK "up_cast_1.typedoo" (Just TyInt),
     TYCK "up_instance_1.typedoo" (Just TyBool),
     TYCK "duplicate_methods_1.typedoo" Nothing,

     TYCK "incomparable_instanceof_2.typedoo" (Just TyInt),
     TYCK "equal_trees_by_double_dispatch.typedoo" (Just TyBool),
     TYCK "goldberg_80.typedoo" (Just (TyListOf TyInt)),
     TYCK "check_interface_implementation_1.typedoo" Nothing,
     TYCK "check_interface_implementation_2.typedoo" Nothing,

     -- with exercise 9.34, this should become an error

     TYCK "bad_cast_1.typedoo" (Just (TyFun [TyClass "c1"] (TyClass "c2"))), --
     TYCK "missing_initialize_method_1.typedoo" Nothing,
     TYCK "duplicate_methods_1.typedoo" Nothing,
     TYCK "incomparable_instanceof_2.typedoo" (Just TyInt),
     
     TYCK "bad_super_1.typedoo" Nothing,
     TYCK "unsupplied_method_2.typedoo" Nothing,
     TYCK "overriding_method_changes_type_1.typedoo" Nothing,
     TYCK "test6_3_1.typedoo" (Just (TyFun [TyClass "c3"] TyInt)), --

     -- here c2 is bad, so the interprter runs successfully and returns
     -- false.
     TYCK "bad_instance_of_1.typedoo" (Just TyBool),
     
     -- here c1 is unrelated to c2, so the interpreter runs
     -- successfully and returns false.
     TYCK "bad_instance_of_2.typedoo" (Just TyBool)

   ]
