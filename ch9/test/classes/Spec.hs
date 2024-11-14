module Spec where

import MainUtil

import Expr
import Test.Hspec
import System.IO (readFile)
import Control.Exception (evaluate)

spec = hspec $ do
  describe "explicitrefslang" $ do
    let atdir f = "./app/explicitrefslang/examples/" ++ f

    mapM_
      (\(name, maybeResStr) -> 
           (it (name) $
              do text <- readFile name
                 
                 case maybeResStr of
                   Just resultStr -> do result <- runProg text False
                                        show result `shouldBe` resultStr
                   Nothing -> (do result <- runProg text False; putStrLn (show result)) `shouldThrow` anyException))
      [ (atdir name, maybeResStr) | (name,maybeResStr) <- testcases ]
         
main = spec

testcases = 
  []