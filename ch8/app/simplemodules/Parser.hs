module Parser where

import CommonParserUtil
import Token
import Expr

-- | Utility
rule prodRule action              = (prodRule, action, Nothing  )
ruleWithPrec prodRule action prec = (prodRule, action, Just prec)

--
parserSpec :: ParserSpec Token AST IO ()
parserSpec = ParserSpec
  {
    startSymbol = "Program'",

    tokenPrecAssoc = [],

    parserSpecList =
    [
      rule "Program' -> Program" (\rhs -> return $ get rhs 1),

      rule "Program -> ZeroOrMoreModuleDefinition Expression" undefined,

      rule "Program -> Expression" undefined,

      rule "ZeroOrMoreModuleDefinition -> " undefined,

      rule "ZeroOrMoreModuleDefinition -> ModuleDefinition ZeroOrMoreModuleDefinition" undefined,

      rule "ModuleDefinition -> module identifier interface Interface body ModuleBody" undefined,

      rule "Interface -> [ ZeroOrMoreDeclaration ]" undefined,

      rule "ZeroOrMoreDeclaration -> " undefined,

      rule "ZeroOrMoreDeclaration -> Declaration ZeroOrMoreDeclaration" undefined,

      rule "Declaration -> identifier : Type" undefined,

      rule "ModuleBody -> [ ZeroOrMoreDefinition ]" undefined,

      rule "Definition -> identifier = Expression" undefined,

      rule "Expression -> from identifier take identifier" undefined,

      rule "Expression -> integer_number"
        (\rhs -> return $ toASTExp $ Const_Exp (read (getText rhs 1) :: Int)),
      
      rule "Expression -> - integer_number"
        (\rhs -> return $ toASTExp $ Const_Exp (-(read (getText rhs 2) :: Int))),
      
      rule "Expression -> - ( Expression , Expression )"
        (\rhs -> return $ toASTExp $ Diff_Exp (fromASTExp $ get rhs 3) (fromASTExp $ get rhs 5)),

      rule "Expression -> zero? ( Expression )"
        (\rhs -> return $ toASTExp $ IsZero_Exp (fromASTExp $ get rhs 3)),
      
      rule "Expression -> if Expression then Expression else Expression"
        (\rhs -> return $ toASTExp $ If_Exp (fromASTExp $ get rhs 2) (fromASTExp $ get rhs 4) (fromASTExp $ get rhs 6)),

      rule "Expression -> identifier" (\rhs -> return $ toASTExp $ Var_Exp (getText rhs 1)),
      
      rule "Expression -> let identifier = Expression in Expression"
        (\rhs -> return $ toASTExp $ Let_Exp (getText rhs 2) (fromASTExp $ get rhs 4) (fromASTExp $ get rhs 6)),

      rule "Expression -> letrec Type identifier ( identifier : Type ) = Expression in Expression"
        (\rhs -> return $ toASTExp $ Letrec_Exp (fromASTType $ get rhs 2) (getText rhs 3) (getText rhs 5) (fromASTType $ get rhs 7) (fromASTExp $ get rhs 10) (fromASTExp $ get rhs 12)),

      rule "Expression -> proc ( identifier : Type ) Expression"
        (\rhs -> return $ toASTExp $ Proc_Exp (getText rhs 3) (fromASTType $ get rhs 5) (fromASTExp $ get rhs 7)),

      rule "Expression -> ( Expression Expression )"
        (\rhs -> return $ toASTExp $ Call_Exp (fromASTExp $ get rhs 2) (fromASTExp $ get rhs 3)),
        
      rule "Type -> int"
        (\rhs -> return $ toASTType $ TyInt),
      
      rule "Type -> bool"
        (\rhs -> return $ toASTType $ TyBool),
      
      rule "Type -> ( Type -> Type )"
        (\rhs -> return $ toASTType $ TyFun (fromASTType (get rhs 2)) (fromASTType (get rhs 4))),

      rule "Type -> identifier" undefined, 

      rule "Type -> from identifier take identifeir" undefined
    ],
    
    baseDir        = "./",
    actionTblFile  = "action_table_simplemodules.txt",
    gotoTblFile    = "goto_table_simplemodules.txt",
    grammarFile    = "prod_rules_simplemodules.txt",
    parserSpecFile = "mygrammar_simplemodules.grm",
    genparserexe   = "yapb-exe"
  }


