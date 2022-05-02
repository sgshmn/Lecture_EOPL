module Main where

import MidExam
import Test.Hspec

import Control.Exception 
import Data.Maybe (isJust)
import System.IO (readFile)

spec = hspec $ do
  describe "Problem 01: Name & ID" $ do
    it ("[name and student id] ") $
      do putStrLn (studentId ++ ", " ++ name)

  describe "Problem 02-1: length" $ do
    it ("lengthList Nil = 0") $
      do lengthList Nil `shouldBe` 0 

    it ("lengthList (Cons 'a' Nil) = 1") $
      do lengthList (Cons 'a' Nil) `shouldBe` 1

    it ("lengthList (Cons 123 Nil) = 1") $
      do lengthList (Cons 123 Nil) `shouldBe` 1

    it ("lengthList (Cons 1 (Cons 2 (Cons 3 Nil))) = 3") $
      do lengthList (Cons 1 (Cons 2 (Cons 3 Nil))) `shouldBe` 3

  describe "Problem 02-2 ; concatList" $ do
    it ("concatList (Cons (Cons 1 (Cons 2 (Cons 3 Nil))) \
                      \   (Cons (Cons 4 Nil) \
                      \    (Cons (Cons 5 (Cons 6 Nil)) Nil))) \
                      \ = Cons 1 (Cons 2 (Cons 3 (Cons 4 \
                      \           (Cons 5 (Cons 6 Nil)))))") $
      do concatList (Cons (Cons (1 :: Int) (Cons 2 (Cons 3 Nil)))
                           (Cons (Cons 4 Nil)
                            (Cons (Cons 5 (Cons 6 Nil)) Nil)))
            `shouldBe` Cons (1 :: Int) (Cons 2 (Cons 3 (Cons 4 (Cons 5 (Cons 6 Nil)))))

  describe "Problem 02-3 : nthElement" $ do
    it ("nthElement (Cons 'a' (Cons 'b' (Cons 'c' Nil))) 1 = 'b'") $
      do nthElement (Cons 'a' (Cons 'b' (Cons 'c' Nil))) 1 `shouldBe` 'b'

    it ("nthElement (Cons 7 (Cons 3 (Cons 9 Nil))) 2 = 9") $
      do nthElement (Cons 7 (Cons 3 (Cons 9 Nil))) 2 `shouldBe` 9

    it ("nthElement (Cons 'a' (Cons 'b' (Cons 'c' Nil))) 3 = index out of range!") $
      do evaluate (nthElement (Cons 'a' (Cons 'b' (Cons 'c' Nil))) 3 :: Char) `shouldThrow` errorCall "index out of range!"

  describe "Problem 02-4 : removeFirst" $ do
    it ("removeFirst 'a' (Cons 'a' (Cons 'b' (Cons 'c' Nil))) = Cons 'b' (Cons 'c' Nil)") $
      do removeFirst 'a' (Cons 'a' (Cons 'b' (Cons 'c' Nil))) `shouldBe` Cons 'b' (Cons 'c' Nil)

    it ("removeFirst 'b' (Cons 'e' (Cons 'f' (Cons 'g' Nil))) = Cons 'e' (Cons 'f' (Cons 'g' Nil))") $
      do removeFirst 'b' (Cons 'e' (Cons 'f' (Cons 'g' Nil))) `shouldBe` Cons 'e' (Cons 'f' (Cons 'g' Nil))

  describe "Problem 02-5 : numberElements" $ do
    it ("numberElements (Cons 'a' (Cons 'b' (Cons 'c' Nil))) = \
                      \ Cons ('a',0) (Cons ('b',1) (Cons ('c',2) Nil))") $
      do numberElements (Cons 'a' (Cons 'b' (Cons 'c' Nil))) `shouldBe`
             Cons ('a',0) (Cons ('b',1) (Cons ('c',2) Nil))


  describe "Problem 03 : treeToList" $ do
    it ("treeToList (...) = ...") $
      do treeToList
          (Node 'F'
            (Node 'B' (Node 'A' Empty Empty)
                       (Node 'D' (Node 'C' Empty Empty) (Node 'E' Empty Empty)))
             (Node 'G' Empty (Node 'I' (Node 'H' Empty Empty) Empty)))
            `shouldBe` Cons 'F' (Cons 'B' (Cons 'A' (Cons 'D' (Cons 'C'
                          (Cons 'E' (Cons 'G' (Cons 'I' (Cons 'H' Nil))))))))

  describe "Problem 04-1 : environment" $ do
    it ("Example environment - 1") $
      do apply_env example_env "x" `shouldBe` (Num_Val 1)

    it ("Example environment - 2") $
      do apply_env example_env "y" `shouldBe` (Num_Val 2)

    it ("Example environment - 3") $
      do apply_env example_env "z" `shouldBe` (Bool_Val True)

  describe "Problem 04-2 : empty_env, apply_env, extend_env" $ do
    it ("Example environment - 1") $
      do apply_env (extend_env "z" (Bool_Val True)
                     (extend_env "y" (Num_Val 2)
                       (extend_env "x" (Num_Val 1) empty_env))) "x"
                              `shouldBe` (Num_Val 1)

    it ("Example environment - 1") $
      do apply_env (extend_env "z" (Bool_Val True)
                     (extend_env "y" (Num_Val 2)
                       (extend_env "x" (Num_Val 1) empty_env))) "y" 
                              `shouldBe` (Num_Val 2)

    it ("Example environment - 1") $
      do apply_env (extend_env "z" (Bool_Val True)
                     (extend_env "y" (Num_Val 2)
                       (extend_env "x" (Num_Val 1) empty_env))) "z"
                              `shouldBe` (Bool_Val True)

    it ("Example environment - 1") $
      do evaluate (apply_env (extend_env "z" (Bool_Val True)
                     (extend_env "y" (Num_Val 2)
                       (extend_env "x" (Num_Val 1) empty_env))) "k") `shouldThrow` errorCall "variable not found!"


  describe "Problem 05-1 : Lambda calculus expressions" $ do
    it ("Example lcexp1 - 1") $
      do occurFree "x" example_lcexp1 `shouldBe` False

    it ("Example lcexp1 - 2") $
      do occurFree "f" example_lcexp1 `shouldBe` False

    it ("Example lcexp1 - 3") $
      do occurFree "g" example_lcexp1 `shouldBe` False


  describe "Problem 05-2 : occursFree for lambda calculus expressions" $ do
    it ("occurFree - 1") $
      do occurFree "x" (Var_exp "x") `shouldBe` True

    it ("occurFree - 2") $
      do occurFree "x" (Var_exp "y") `shouldBe` False

    it ("occurFree - 3") $
      do occurFree "x" (Lambda_exp "x" (App_exp (Var_exp "x") (Var_exp "y"))) `shouldBe` False

    it ("occurFree - 4") $
      do occurFree "x" (Lambda_exp "y" (App_exp (Var_exp "x") (Var_exp "y"))) `shouldBe` False

    it ("occurFree - 5") $
      do occurFree "x" (App_exp (Lambda_exp "x" (Var_exp "x")) (App_exp (Var_exp "x") (Var_exp "y"))) `shouldBe` True

    it ("occurFree - 6") $
      do occurFree "x" (Lambda_exp "y" (Lambda_exp "z" (App_exp (Var_exp "x") (App_exp (Var_exp "y") (Var_exp "z"))))) `shouldBe` True




main :: IO ()
main = spec
