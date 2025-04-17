module Comp where 

import qualified Expr as R 
import qualified Actors.Expr as A  

import qualified Data.Map as Map

type Identifier = String
type Location = String
type SEnv = Map.Map Identifier Identifier 
type Cont = A.Exp
type Table = Map.Map Integer ([Identifier], Identifier, A.Exp)  -- (y1,...,yk, x, e)

comp :: R.Exp -> Location -> SEnv -> Cont -> Integer -> Table -> (A.Exp, Integer, Table)
comp (R.Var_Exp x) loc senv k n tbl = 
    let y = apply_senv senv x in 
        (A.Call_Exp k (A.Var_Exp y), n, tbl)
    
comp (R.Proc_Exp locb x e) loc senv k n tbl
    | locb == loc = undefined  -- local procedure call
    | otherwise = undefined    -- remote procedure call
    
comp (R.Call_Exp e1 locb e2) loc senv k n tbl = undefined

comp (R.Const_Exp n) _ _ _ _ _ = undefined
comp (R.Diff_Exp e1 e2) loc senv k n tbl = undefined
comp (R.IsZero_Exp e) loc senv k n tbl = undefined
comp (R.If_Exp e1 e2 e3) loc senv k n tbl = undefined
comp (R.Let_Exp x e1 e2) loc senv k n tbl = undefined

--
apply_senv :: SEnv -> Identifier -> Identifier
apply_senv senv x = case Map.lookup x senv of
    Just y  -> y
    Nothing -> error ("unbound variable: " ++ x)
