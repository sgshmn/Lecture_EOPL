module Main where

import MainUtil

import Expr
import Test.Hspec
import System.IO (readFile)

spec = hspec $ do
  describe "letlang" $ do
    let atdir f = "./app/letlang/examples/" ++ f

    let name = "if_true.let"
    let benchmark = atdir name
    let dotresult = atdir (name ++ ".result")
    
    it (name) $
      do text <- readFile benchmark
         result <- runProg text
         resultStr <- readFile dotresult
         show result `shouldBe` resultStr
         
         
main = spec
