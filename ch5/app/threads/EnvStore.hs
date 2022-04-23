module EnvStore where

import Ref (Location)
import Expr (Identifier,Exp)
import Data.List(intersperse)

-- Environment
data Env =
    Empty_env
  | Extend_env Identifier ExpVal Env
  | Extend_env_rec Identifier Identifier Exp Env

empty_env :: Env
empty_env = Empty_env

apply_env :: Env -> Identifier -> ExpVal
apply_env Empty_env search_var = error (search_var ++ " is not found.")
apply_env (Extend_env saved_var saved_val saved_env) search_var
  | search_var==saved_var = saved_val
  | otherwise             = apply_env saved_env search_var
apply_env (Extend_env_rec p_name b_var p_body saved_env) search_var
  | p_name==search_var = Proc_Val (procedure b_var p_body (Extend_env_rec p_name b_var p_body saved_env))
  | otherwise          = apply_env saved_env search_var

extend_env :: Identifier -> ExpVal -> Env -> Env
extend_env x v env = Extend_env x v env

extend_env_rec :: Identifier -> Identifier -> Exp -> Env -> Env
extend_env_rec f x exp env = Extend_env_rec f x exp env

-- Expressed values
data ExpVal =
    Num_Val {expval_num :: Int}
  | Bool_Val {expval_bool :: Bool}
  | Proc_Val {expval_proc :: Proc}
  | List_Val {expval_list :: [ExpVal]}

instance Show ExpVal where
  show (Num_Val num)   = show num
  show (Bool_Val bool) = show bool
  show (Proc_Val proc) = show "<proc>"
  show (List_Val nums) = show "[" ++ concat (intersperse "," (map show nums)) ++ show "]"

-- Denoted values
type DenVal = ExpVal   

-- Procedure values : data structures
data Proc = Procedure {var :: Identifier, body :: Exp, saved_env :: Env}

procedure :: Identifier -> Exp -> Env -> Proc
procedure var body env = Procedure var body env

-- In Interp.hs
-- apply_procedure :: Proc -> ExpVal -> ExpVal
  
type Store = (Location, [(Location,ExpVal)]) -- Next new location

newref :: Store -> ExpVal -> (Location,Store)
newref store@(next,s) v = (next,(next+1,(next,v):s))

deref :: Store -> Location -> ExpVal
deref store@(next,s) loc =
  case [v | (loc',v) <- s, loc==loc'] of
    (v:_) -> v
    _     -> error ("Location not found: " ++ show loc)

setref :: Store -> Location -> ExpVal -> Store
setref store@(next,s) loc v = (next,update s)
  where update [] = error ("Invalid reference: " ++ show loc)
        update ((loc',w):s')
          | loc==loc' = (loc,v):s'
          | otherwise = (loc',w):update s'

initStore :: Store
initStore = (1,[])

