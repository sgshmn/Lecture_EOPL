module Expr where

type Program = Exp
  
data Exp =
    Const_Exp     Int
  | Add_Exp       Exp Exp
  | Diff_Exp      Exp Exp
  | Mul_Exp       Exp Exp
  | Quot_Exp      Exp Exp              -- 몫
  | IsZero_Exp    Exp
  | IsEqual_Exp   Exp Exp
  | IsGreater_Exp Exp Exp
  | IsLess_Exp    Exp Exp
  | If_Exp        Exp Exp Exp
  | Var_Exp       Identifier
  | Let_Exp       Identifier Exp Exp
  deriving Show

type Identifier = String

data ExpVal =
    Num_Val {expval_num :: Int}
  | Bool_Val {expval_bool :: Bool}
  deriving Eq               -- for testing

instance Show ExpVal where
  show (Num_Val num)   = show num
  show (Bool_Val bool) = show bool

type DenVal = ExpVal   
