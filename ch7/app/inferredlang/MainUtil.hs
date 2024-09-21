module MainUtil where

import CommonParserUtil

import TokenInterface
import Lexer
import Terminal
import Parser
import Expr
import Interp
import TypeCheck
import TypeInfer
import Subst

import Control.Monad (when)
import System.IO
import System.Environment (getArgs, withArgs)

parser text = do
  parsing False                            -- parser converting a text-based program
     parserSpec ((), 1, 1, text)           -- into a program in abstract syntax tree (Expr)
     (aLexer lexerSpec)
     (fromToken (endOfToken lexerSpec))

run text = do
  val <- runProg text True
  putStrLn (show val)

runProg text bool = do 
  expressionAst <- parser text
  let expression = fromASTExp expressionAst

  if bool then putStrLn (show expression) else return ()
  
  let val = value_of_program expression      -- interpreter
  return val 

typeinference text = do
  let debugFlag = False
        
  expressionAst <-
    parsing debugFlag
       parserSpec ((), 1, 1, text)
       (aLexer lexerSpec)
       (fromToken (endOfToken lexerSpec))

  let expression = fromASTExp expressionAst
  
  putStrLn (show expression)

  eitherTySubstOrErr <- typeInfer expression
  case eitherTySubstOrErr of
    Right (ty,subst) ->
      do putStrLn (show (apply_subst_to_type ty subst))

         let val = value_of_program expression
         putStrLn (show val)

    Left errMsg ->
      do putStrLn errMsg
