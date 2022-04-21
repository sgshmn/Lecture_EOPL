{-# LANGUAGE FlexibleInstances #-}

-- [MID-TERM EXAMINATION]
--
--  - Solve the problems, and submit <midexamp-0.1.0.0.tar.gz> to E-class!
--    (문제를 풀고 <midexampl-0.1.0.0.tar.gz>를 E-class에 제출하면 됩니다.)
--
--  - You are only required to rewrite <error "TODO" ...> in Main.hs, and
--    do not modify anything elsewhere.
--
--  - Main.hs에서 error "TODO: .." 부분만 작성하시고 그 외의 부분은
--    수정하지 마세요.
--
--  - $ stack sdist
--    ...
--    Wrote sdist tarball to /home/khchoi/work/lecture/pl/exams/midexam/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.4.1.0/midexam-0.1.0.0.tar.gz
--                                                                                                                      ^^^^^^^^^^^^^^^^^^^^^^^^
--    ...
--
--
-- +--!Warning! |--------------------------------------------------------------+
-- |                                                                           |
-- |  - DO NOT DISCUSS WITH OTHER STUDENTS ABOUT THE MID-TERM TAKE-HOME EXAM!  |
-- |    (중간고사 Take-home 시험은 스스로 해결하시고, 다른 학생과 논의하지 마세요!)  |
-- |                                                                           |
-- +---------------------------------------------------------------------------+

module Main where

--------------------------------------------------------------------------------
-- [Problem 01]
--------------------------------------------------------------------------------
--
--   Write your student id and name. (학번과 이름을 작성하시오.)
--
--------------------------------------------------------------------------------

name :: String
name = error "TODO: implement name"

studentId :: String
studentId = error "TODO: implement studentId"

main :: IO ()
main = putStrLn ("Mid-term examination(중간고사): " ++ studentId ++ ", " ++ name)


--------------------------------------------------------------------------------
-- [Problem 02]
--------------------------------------------------------------------------------
--
-- (02-1) Write a polymorphic length function for List a.
--        (다형 타입 List a에 대한 길이 함수를 작성하시오.)
--
--   Examples) lengthList Nil = 0
--
--             lengthList (Cons 'a' Nil) = 1
--
--             lengthList (Cons 123 Nil) = 1
--
--             lengthList (Cons 1 (Cons 2 ( ... (Cons 10 Nil)... ))) = 10
--------------------------------------------------------------------------------

data List a = Nil | Cons a (List a) deriving Show

lengthList :: List a -> Int
lengthList = error "TODO: implement a lengthList function"


--------------------------------------------------------------------------------
-- (02-2) Write a polymorphic concatList function for List a.
--        (다형 타입 List a에 대한 concatList 함수를 작성하시오.)
--
--        cf. Haskell's concat
--                concat [ [1,2,3], [4], [5,6] ] = [1,2,3,4,5,6]
--
--        Examples) concatList
--                    (Cons 1 (Cons 2 (Cons 3 Nil)))
--                     (Cons 4 Nil)
--                      (Cons 5 (Cons 6 Nil))
--                   = Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 (Cons 6 Nil))))) 
--------------------------------------------------------------------------------

concatList :: List (List a) -> List a
concatList = error "TODO: implement a concatList function"


--------------------------------------------------------------------------------
-- (02-3) Write a polymorphic nthElement function for List a.
--        (다형 타입 List a에 대한 nthElement 함수를 작성하시오.)
--
--        Examples) nthElement (Cons 'a' (Cons 'b' (Cons 'c' Nil))) 1
--                  = 'b' 
--
--                  nthElement (Cons 'a' (Cons 'b' (Cons 'c' Nil))) 3
--                  ===> error!!
--
--                  nthElement (Cons 7 (Cons 3 (Cons 9 Nil))) 2
--                  = 9

nthElement :: List a -> Int -> a
nthElement = error "TODO: implement a nthElement function"


--------------------------------------------------------------------------------
-- (02-4) Write a polymorphic removeFirst function for List a.
--        (다형 타입 List a에 대한 removeFirst 함수를 작성하시오.)
--
--        Examples) removeFirst 'a' (Cons 'a (Cons 'b' (Cons 'c' Nil)))
--                  = Cons 'b' (Cons 'c' Nil)
--
--                  removeFirst 'b' (Cons 'e' (Cons 'f' (Cons 'g' Nil)))
--                  = Cons 'e' (Cons 'f' (Cons 'g' Nil))

removeFirst :: Eq a => a -> List a -> List a
removeFirst = error "TODO: implement a removeFirst function"

--------------------------------------------------------------------------------
-- (02-5) Write a polymorphic numberElements function for List a.
--        (다형 타입 List a를 받아 0부터 번호를 매겨 원소와 번호를
--        쌍으로 하는 리스트를 리턴하는 함수를 작성하시오.)
--
--        Examples) numberElements (Cons 'a' (Cons 'b' (Cons 'c' Nil)))
--                  = Cons ('a',0) (Cons ('b',1) (Cons ('c',2) Nil))
-- 
--  Hint: Introduce an auxiliary function.
--

numberElements :: List a -> List (a,Int)
numberElements = error "TODO: implement a numberElements function"


--------------------------------------------------------------------------------
-- [Problem 03]
--------------------------------------------------------------------------------
--
-- Write a treeToList to convert a tree of type Tree a into a list of
-- type List a.
-- (다형 타입 Tree a를 받아 List a로 변환하는 treeToList 함수를
-- 작성하시오.)
--

data Tree a = Node a (Tree a) (Tree a)
            | Leaf a
   deriving Show

treeToList :: Tree a -> List a
treeToList = error "TODO: implement treeToList"


--------------------------------------------------------------------------------
-- [Problem 04]
--------------------------------------------------------------------------------
--
-- In Exercise3, there is a problem with isSearch and a test with
-- isSearchTreeProp as summarized as follows.
-- (연습문제3에 isSearch 문제가 있고 isSearchTreeProp로 검사한다.)
-- 
--   isSearch is in src/Exercse03.hs
--
--     The problem was to Implement a function that determines whether
--     of not a given tree is a search tree (is a "sorted" tree) NOTE:
--     this means for every Node v t0 t1, v is >= all values in tree t0
--     and v is < all values in tree t1 start by defining functions that
--     test if a value is less/greater than all the values in a tree
--
--   isSearchTreeProp is in test/Spec.hs
--
--     isSearchTreeProp :: Exercise3.Tree Int -> Bool
--     isSearchTreeProp ts =
--       let
--         isSorted xs = and . map (\(x,y) -> x <= y) . zip xs $ tail xs
--         isSearch    = isSorted $ flatten ts
--       in isSearch == Exercise3.isSearchTree ts
--
-- (04-1) Clarify why this specification is incorrect with repsect to
--        the problem statement?
--        (isSearchTreeProp의 검사 방법이 문제에 기술된 바와 비교해서
--        어떤 점이 틀렸는지 간단히 설명하시오.)
--

reasonFortheIncorrectSpec :: String
reasonFortheIncorrectSpec = "TODO: write a reason(이유를 작성하세요.)"


-- (04-2) Write a isSearch function. (isSearch 함수를 작성하시오.)
--

isSearchTree :: Ord a => Tree a -> Bool
isSearchTree tree = error "TODO implement isSearchTree"


--------------------------------------------------------------------------------
-- [Problem 05]
--------------------------------------------------------------------------------
--
-- Environments are a data structure to know the value associated with
-- each variable. Implement three interface functions for
-- environments.
--
-- (환경-Environment은 프로그램의 변수가 가지고 있는 값이 무엇인지
-- 알고자 할 때 유용한 자료구조이다. 이 환경의 3가지 인터페이스 함수를
-- 구현하시오.)
--

type Identifier = String           -- identifier 식별자 (변수명)

data ExpVal =                      -- 값: 숫자 또는 부울 
    Num_Val {expval_num :: Int}
  | Bool_Val {expval_bool :: Bool}

type Env = [(Identifier,ExpVal)]


-- (05-1) Write an environement for x ===> 1
--                                  y ===> 2
--                                  z ===> True
--

example_env = error "TODO write an example environment"


-- (05-2) Implement empty_env, apply_env, and extend_env.
--
--   - (empty_env) returns the empty list as the empty environment.
--
--   - (extend_env x v env) adds a new binding (association) of x and
--     v to env.
--
--       Examples) the environment example in (05-1) can be built by the
--                 three interfaces as:
--
--          example_env = extend_env "z" (Bool_Val True)
--                           (extend_env "y" (Num_Val 2)
--                               (extend_env "x" (Num_Val 1)
--                                   empty_env))
--
--   - (apply_env env x) returns the value v where (x,v) is in env and
--     it is the first pair having x as its identifier.
--
--     If not found, (apply_env env x) produces an error.
--
--       Examples)  apply_env example_env "y"
--                  = Num_Val 2
--
--                  apply_env example_env "a"
--                  ===> error!
--

empty_env :: Env
empty_env = error "TODO: implement empty_env"

apply_env :: Env -> Identifier -> ExpVal
apply_env env x = error "TODO implement apply_env"

extend_env :: Identifier -> ExpVal -> Env -> Env
extend_env x v env = error "TODO implement extend_env"



--------------------------------------------------------------------------------
-- [Problem 06]
--------------------------------------------------------------------------------
--
--  A recursive data type for lambda calculus expressions is defined as follows.
--

data Lc_exp = Var_exp    Identifier
            | Lambda_exp Identifier Lc_exp
            | App_exp    Lc_exp     Lc_exp
            deriving Show


-- Lambda expressions in Haskell can be expressed by Lc_exp.
--
--      Examples)     \x -> x              Lambda_exp "x" (Var_exp "x")
--
--                    \x -> \y -> x        Lambda_exp "x" (Lambda_exp "y" (Var_exp "x"))
--                    (or  \x y -> x)
--
--                    (\x -> x) z          App_exp (Lambda_exp "x" (Var_exp "x")) (Var_exp "z")
--
-- (06-1) Write the following Haskell lambda expressions in Lc_exp.
--

example_lcexp1 :: Lc_exp
example_lcexp1 = error "TODO: write Lc_exp for \f g x -> f x (g x)"

-- (06-2) Write a occursFree function.
--
--  Examples)
--
--    Does x occurrs free in x? yes!
--
--         occurFree "x" (Var_exp "x") = True
--
--    Does x occurrs free in y? no!
--
--         occurFree "x" (Var_exp "y") = False
--
--    Does x occurrs free in \x -> x y? no!
--
--         occurFree "x" (Lambda_exp "y"
--                           (App_exp
--                               (Var_exp "x")
--                               (Var_exp "y"))) = False

--    Does x occurrs free in \y -> x y? yes!
--
--         occurFree "x" (Lambda_exp "y"
--                           (App_exp
--                               (Var_exp "x")
--                               (Var_exp "y"))) = True
--
--    Does x occurrs free in (\x -> x) (x y)? yes!
--
--         occurFree "x" (App_exp
--                         (Lambda_exp "x" (Var_exp "x"))
--                         (App_exp (Var_exp "x") (Var_exp "y"))) = True
-- 
--    Does x occurs free in \y -> \z -> (x (y z))? yes!
--
--         occurFree "x" (Lambda_exp "y"
--                          (Lambda_exp "z"
--                             (App_exp (Var_exp "x")
--                                      (App_exp (Var_exp "y")
--                                               (Var_exp "z"))))) = True



occurFree :: Identifier -> Lc_exp -> Bool
occurFree = error "TODO: write occurFree for Lc_exp"



