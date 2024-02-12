module Parser where

import CommonParserUtil
import Token
import Expr

-- | Utility
rule prodRule action              = (prodRule, action, Nothing  )
ruleWithPrec prodRule action prec = (prodRule, action, Just prec)

--
parserSpec :: ParserSpec Token PET IO ()
parserSpec = ParserSpec
  {
    startSymbol = "Program'",

    tokenPrecAssoc = [],

    parserSpecList =
    [
      rule "Program' -> Program" (\rhs -> return $ get rhs 1),

      rule "Program -> ZeroMoreClassDecl Expression" (\rhs -> return $ undefined),

      rule "ZeroMoreClassDecl -> ClassDecl ZeroMoreClassDecl" (\rhs -> return $ undefined),

      rule "ZeroMoreClassDecl -> " (\rhs -> return $ undefined),

      rule "ClassDecl -> class identifier extends identifier { ZeroMoreFieldDecl ZeroMoreMethodDecl }"
        (\rhs -> return $ undefined),

      rule "ZeroMoreFieldDecl -> identifier ZeroMoreFieldDecl" (\rhs -> return $ undefined),

      rule "ZeroMoreFieldDecl -> " (\rhs -> return $ undefined),

      rule "ZeroMoreMethodDecl -> MethodDecl ZeroMoreMethodDecl" (\rhs -> return $ undefined),

      rule "ZeroMoreMethodDecl -> " (\rhs -> return $ undefined),

      rule "MethodDecl -> method identifier ( ZeroMoreIdentifier ) Expression"
        (\rhs -> return $ undefined),

      rule "ZeroMoreIdentifier -> OneMoreIdentifier" (\rhs -> return $ undefined),

      rule "ZeroMoreIdentifier -> " (\rhs -> return $ undefined),

      rule "OneMoreIdentifier -> identifier , OneMoreIdentifier" (\rhs -> return $ undefined),

      rule "OneMoreIdentifier -> identifier" (\rhs -> return $ undefined),

      rule "ZerMoreExpression -> OneMoreExpression" (\rhs -> return $ undefined),

      rule "ZerMoreExpression -> " (\rhs -> return $ undefined),

      rule "OneMoreExpression -> Expression , OneMoreExpression" (\rhs -> return $ undefined),

      rule "OneMoreExpression -> Expression" (\rhs -> return $ undefined),

      rule "Expression -> new identifier ( ZeroMoreExpression )"
        (\rhs -> return $ undefined),

      rule "Expression -> self" (\rhs -> return $ undefined),

      rule "Expression -> send Expression identifier ( ZeroMoreExpression )"
        (\rhs -> return $ undefined),

      rule "Expression -> super identifier ( ZeroMoreExpression )"
        (\rhs -> return $ undefined),

      rule "Expression -> integer_number"
        (\rhs -> return $ fromExp $ Const_Exp (read (getText rhs 1) :: Int)),
      
      rule "Expression -> - integer_number"
        (\rhs -> return $ fromExp $ Const_Exp (-(read (getText rhs 2) :: Int))),
      
      rule "Expression -> - ( Expression , Expression )"
        (\rhs -> return $ fromExp $ Diff_Exp (expFrom (get rhs 3)) (expFrom (get rhs 5))),

      rule "Expression -> zero? ( Expression )"
        (\rhs -> return $ fromExp $ IsZero_Exp (expFrom (get rhs 3))),
      
      rule "Expression -> if Expression then Expression else Expression"
        (\rhs -> return $ fromExp $ If_Exp (expFrom (get rhs 2)) (expFrom (get rhs 4)) (expFrom (get rhs 6))),

      rule "Expression -> identifier" (\rhs -> return $ fromExp $ Var_Exp (getText rhs 1)),
      
      rule "Expression -> let identifier = Expression in Expression"
        (\rhs -> return $ fromExp $ Let_Exp (getText rhs 2) (expFrom (get rhs 4)) (expFrom (get rhs 6))),

      rule "Expression -> letrec LetRecBindings in Expression"
        (\rhs -> return $ fromExp $ Letrec_Exp (idIdExpListFrom (get rhs 2)) (expFrom (get rhs 4))),

      rule "Expression -> proc ( identifier ) Expression"
        (\rhs -> return $ fromExp $ Proc_Exp (getText rhs 3) (expFrom (get rhs 5))),

      rule "Expression -> ( Expression Expression )"
        (\rhs -> return $ fromExp $ Call_Exp (expFrom (get rhs 2)) (expFrom (get rhs 3))),

      rule "Expression -> begin ExpressionList end"
        (\rhs -> return $ fromExp $ Block_Exp (expListFrom (get rhs 2))),

      rule "Expression -> set identifier = Expression"
        (\rhs -> return $ fromExp $ Set_Exp (getText rhs 2) (expFrom (get rhs 4))),

      rule "LetRecBindings -> identifier ( identifier ) = Expression"
        (\rhs -> return $ fromIdIdExpList [(getText rhs 1, getText rhs 3, expFrom (get rhs 6))]),

      rule "LetRecBindings -> identifier ( identifier ) = Expression LetRecBindings"
        (\rhs -> return $ fromIdIdExpList ((getText rhs 1, getText rhs 3, expFrom (get rhs 6)) : idIdExpListFrom (get rhs 7))),

      rule "ExpressionList -> Expression"
        (\rhs -> return $ fromExpList $ [ expFrom (get rhs 1) ]),

      rule "ExpressionList -> Expression ; ExpressionList"
        (\rhs -> return $ fromExpList $ (expFrom (get rhs 1) : expListFrom (get rhs 3)))        
    ],
    
    baseDir        = "./",
    actionTblFile  = "action_table_classes.txt",
    gotoTblFile    = "goto_table_classes.txt",
    grammarFile    = "prod_rules_classes.txt",
    parserSpecFile = "mygrammar_classes.grm",
    genparserexe   = "yapb-exe"
  }


