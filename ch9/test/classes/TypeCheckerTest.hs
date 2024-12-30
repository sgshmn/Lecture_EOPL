{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module TypeCheckerTest where

import Expr -- (Type)
import Testcase

typechecker_tests :: TestSuite
typechecker_tests =
  TestSuite
   [
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
     RUN "apply_proc_in_rator_pos.proc" (Just "29"),
     RUN "apply_simple_proc.proc" (Just "29"),
     RUN "let_to_proc_1.proc" (Just "29"),

     RUN "nested_procs_1.proc" (Just "-1"),
     RUN "nested_procs_2.proc" (Just "-1"),

     RUN "y_combinator_1.proc" (Just "12"),

     -- simple letrecs
     RUN "simple_letrec_1.letrec" (Just "32"),
     RUN "simple_letrec_2.letrec" (Just "8"),
     RUN "simple_letrec_3.letrec" (Just "20"),
     
     RUN "ho_nested_letrecs.letrec" (Just "1"),
     RUN "begin_test_1.letrec_ext" (Just "3"),

     RUN "assignment_test_1.impref" (Just "27"),
     RUN "gensym_test_1.impref" (Just "-1"),
     RUN "even_odd_via_set_1.impref" (Just "1"),
     RUN "example_for_book_1.impref" (Just "12"),

     -- multiple arguments
     RUN "nested_procs_2.proc" (Just "-1"),
     RUN "twice_cps.proc" (Just "9"),
     RUN "cps_neither_basic.proc" (Just "17"),

     RUN "create_empty_class.classes" (Just "3"),
     RUN "create_class_with_method.classes" (Just "33"),
     RUN "create_object.classes" (Just "11"),

     RUN "send_msg_1.classes" (Just "44"),
     RUN "send_msg_2.classes" (Just "(44 33)"),

     RUN "test_self_1.classes" (Just "(11 13)"),
     RUN "counter_1.classes" (Just "(0 1)"),
     RUN "shared_counter_1.classes" (Just "((1 3) (2 3))"),
     RUN "chris_1.classes" (Just "5"),
     RUN "for_book_1.classes" (Just "((3 -3) (5 -5))"),
     RUN "odd_even_via_self.classes" (Just "1"),
     
     RUN "inherit_1.classes" (Just "33"),
     RUN "inherit_2.classes" (Just "(33 (2 1) 2 2)"),
     RUN "inherit_3.classes" (Just "(3 2 1)"),
     RUN "chris_2.classes" (Just "(2 2)"),
     RUN "for_book_2.classes" (Just "(1 100 100 1 2 2)"),
     RUN "sum_leaves.classes" (Just "12"),
     RUN "check_shadow_fields.classes" (Just "(1 (2 0) 3)"),
     RUN "static_super.classes" (Just "33"),
     RUN "every_concept.classes" (Just "((50 35 35) (35 (15 100 200) (15 100 200)) (300 35 35))"),
     RUN "colorpoint_1.classes" (Just "((6 8) (20 40) 87)"),
     RUN "colorpoint_2.classes" (Just "((6 8) (20 40) 87)"),
     RUN "example_for_impl.classes" (Just "-10")

   ]
