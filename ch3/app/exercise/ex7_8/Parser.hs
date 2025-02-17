module Parser where

import CommonParserUtil
import Token
import Expr

-- | Utility
rule prodRule action              = (prodRule, action, Nothing  )
ruleWithPrec prodRule action prec = (prodRule, action, Just prec)

--
parserSpec :: ParserSpec Token Exp IO ()
parserSpec = ParserSpec
  {
    startSymbol = "Expression'",

    tokenPrecAssoc = [],

    parserSpecList =
    [
      rule "Expression' -> Expression" (\rhs -> return $ get rhs 1),

      rule "Expression -> integer_number"
        (\rhs -> return $ Const_Exp (read (getText rhs 1) :: Int)),
      
      rule "Expression -> - integer_number"
        (\rhs -> return $ Const_Exp (-(read (getText rhs 2) :: Int))),
      
      rule "Expression -> + ( Expression , Expression )"
        (\rhs -> return $ Add_Exp (get rhs 3) (get rhs 5)),

      rule "Expression -> - ( Expression , Expression )"
        (\rhs -> return $ Diff_Exp (get rhs 3) (get rhs 5)),

      rule "Expression -> * ( Expression , Expression )"
        (\rhs -> return $ Mul_Exp (get rhs 3) (get rhs 5)),

      rule "Expression -> / ( Expression , Expression )"
        (\rhs -> return $ Quot_Exp (get rhs 3) (get rhs 5)),

      rule "Expression -> zero? ( Expression )"
        (\rhs -> return $ IsZero_Exp (get rhs 3)),

      rule "Expression -> equal? ( Expression , Expression )"
        (\rhs -> return $ IsEqual_Exp (get rhs 3) (get rhs 5)),

      rule "Expression -> greater? ( Expression , Expression )"
        (\rhs -> return $ IsGreater_Exp (get rhs 3) (get rhs 5)),

      rule "Expression -> less? ( Expression , Expression )"
        (\rhs -> return $ IsLess_Exp (get rhs 3) (get rhs 5)),
      
      rule "Expression -> if Expression then Expression else Expression"
        (\rhs -> return $ If_Exp (get rhs 2) (get rhs 4) (get rhs 6)),

      rule "Expression -> identifier" (\rhs -> return $ Var_Exp (getText rhs 1)),
      
      rule "Expression -> let identifier = Expression in Expression"
        (\rhs -> return $ Let_Exp (getText rhs 2) (get rhs 4) (get rhs 6))
    ],
    
    baseDir        = "./",
    actionTblFile  = "action_table_let7_8lang.txt",
    gotoTblFile    = "goto_table_let7_8lang.txt",
    grammarFile    = "prod_rules_let7_8lang.txt",
    parserSpecFile = "mygrammar_let7_8lang.grm",
    genparserexe   = "yapb-exe"
  }


