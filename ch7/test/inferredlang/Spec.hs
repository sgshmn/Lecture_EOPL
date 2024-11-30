module Main where

import Expr
import TypeCheck
import Subst
import TypeInfer
import TypeCheckerTest
import Testcase
  
import CommonParserUtil
import TokenInterface
import Lexer
import Parser

import Interp (value_of_program)

import Data.Either(isLeft)
import Test.Hspec

main :: IO ()
main = 
  hspec $ do 
    describe "inferredlang" $ do
      let atdir f = "inferredlang:" ++ f
      let TestSuite typechecker_tests' = typechecker_tests

      mapM_ 
       (\tdtcArg -> (it (atdir(tcname tdtcArg)) $ do doTest tdtcArg))
       typechecker_tests'

fileNameToExpr :: String -> IO Exp
fileNameToExpr tcname =
  do let atdir f = "./app/inferredlang/examples/" ++ f
     exprText <- readFile (atdir tcname)
     textToExpr exprText

textToExpr :: String -> IO Exp
textToExpr exprText =
  do expressionAst <-
          parsing False
              parserSpec ((), 1, 1, exprText)
              (aLexer lexerSpec)
              (fromToken (endOfToken lexerSpec))

     return (fromASTExp expressionAst)

doTest (TDTC tcname exprText maybeResult) =
  do expression <- textToExpr exprText
     typeTest tcname expression maybeResult

doTest (TYCK tcname maybeResult) = 
  do expr <- fileNameToExpr tcname
     typeTest tcname expr maybeResult 

doTest (RUN tcname maybeResult) =
  do expr <- fileNameToExpr tcname
     runTest tcname expr maybeResult 

typeTest tcname expression maybeResult =
  do  -- Just add the type of x!
      case maybeResult of 
        Just ty' ->
          do eitherTyOrErr <- typeInfer (Let_Exp "x" (Const_Exp 1) expression)
             case eitherTyOrErr of
              Left errMsg -> Left errMsg `shouldBe`  Right ty'                   
              Right (ty_,subst) -> 
                let ty = apply_subst_to_type ty_ subst in
                if equal_up_to_typevars ty ty'
                    then ty `shouldBe` ty
                    else ty `shouldBe` ty'
        Nothing ->
          do eitherTyOrErr <- typeInfer (Let_Exp "x" (Const_Exp 1) expression)
             case eitherTyOrErr of
              Left errMsg -> errMsg `shouldBe` errMsg
              Right (ty,_) -> show ty `shouldBe` "some type error"

runTest tcname expression maybeResult =
  do  -- do not typecheck, just run the program
      case maybeResult of
        Just resultStr -> do let result = value_of_program expression
                             show result `shouldBe` resultStr
        Nothing -> (return $ value_of_program expression) 
                     `shouldThrow` anyException