module Testcase where

import Expr(Type)

-- for testing the type checker
type TestCaseName = String
type ExprText = String

data TypeDeclTestCase =
    -- e.g., TDTC "simple-let-1" "let x = 3 in x" (Just TyInt)
    TDTC TestCaseName ExprText (Maybe Type)

    -- e.g., TC "TYCHK_simple-let-1.checked"
  | TYCK TestCaseName (Maybe Type)             

nameMaybeResult (TDTC tcname _ maybeResult) = (tcname, maybeResult)
nameMaybeResult (TYCK tcname maybeResult)   = (tcname, maybeResult)

data TypeDeclTestSuite =
  TypeDeclTestSuite [ TypeDeclTestCase ]
