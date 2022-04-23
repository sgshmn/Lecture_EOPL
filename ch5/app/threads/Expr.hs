module Expr(Program,Exp(..),Identifier,UnaryOp(..)) where

type Program = Exp
  
data Exp =
    Const_Exp  Int
  | Diff_Exp   Exp Exp
  | If_Exp     Exp Exp Exp
  | Var_Exp    Identifier
  | Let_Exp    Identifier Exp Exp
  | Letrec_Exp Identifier Identifier Exp Exp -- letrec f(x) = ... recusive expr ...
  | Proc_Exp   Identifier Exp                -- proc
  | Call_Exp   Exp Exp                       -- call
  | Block_Exp  [ Exp ]                       -- begin exp1; ...; expk end
  | Set_Exp    Identifier Exp                -- set x = exp
  | Spawn_Exp  Exp                           -- spawn exp
  | Yield_Exp                                -- yield ()
  | Mutex_Exp                                -- mutex ()
  | Wait_Exp  Exp                            -- wait exp
  | Signal_Exp  Exp                          -- signal exp
  | Const_List_Exp   [Int]                   -- number list
  | Unary_Exp  UnaryOp Exp                   -- null?, car, cdr
  | Try_Exp    Exp Identifier Exp            -- try exp catch exn exp
  | Raise_Exp  Exp                           -- raise exp
  deriving Show

data UnaryOp = IsZero | IsNull | Car | Cdr | Print deriving Show

type Identifier = String

