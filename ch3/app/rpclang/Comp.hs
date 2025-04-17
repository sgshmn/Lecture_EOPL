{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Comp where 

import qualified Expr as R 
import qualified Actors.Expr as A  

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Control.Applicative as A

type Identifier = String
type Location = String
type SEnv = Map.Map Identifier Identifier 
type Cont = A.Exp -> Integer -> Table -> (A.Exp, Integer, Table)
type Table = Map.Map Integer ([Identifier], Identifier, A.Exp)  -- (y1,...,yk, x, e)

constCREATECLO, constCALLCLO :: String
constCREATECLO = "CREATECLO"
constCALLCLO   = "CALLCLO"

comp :: R.Exp -> Location -> SEnv -> Integer -> Table -> Cont -> (A.Exp, Integer, Table)
comp (R.Var_Exp x) loc senv n tbl k = 
    let y = apply_senv senv x in
        k (A.Var_Exp y) n tbl
    
comp (R.Proc_Exp locb x e) loc senv n tbl k =
    let fnum = n
        fName = "f" ++ show fnum
        fvs = Set.toList (Set.delete x (R.fv e))
        fvsName = "fvs" ++ show (n+1)
        x1 = x1 ++ show (n+2)
        cloName = "clo" ++ show (n+3)
        (body, n1, tbl1) = comp e locb (Map.insert x x1 senv) (n+4) tbl k 
        fclosed = mkProc fvsName fvs x1 body
        tbl2 = Map.insert fnum (fvs, x1, fclosed) tbl1
    in
        if loc == locb then
            k ( A.Call_Exp 
                    (A.Var_Exp fName) 
                    (A.Tuple_Exp (map A.Var_Exp fvs)) ) n1 tbl2
        else
            let (contExp, n2, tbl3) = k (A.Var_Exp cloName) n1 tbl2 in
                (A.Block_Exp [
                    A.Send_Exp [
                        A.Var_Exp locb, A.Var_Exp constCREATECLO, 
                            A.Tuple_Exp [
                                A.Const_Exp (fromInteger fnum),
                                A.Tuple_Exp (map A.Var_Exp fvs),
                                A.Var_Exp loc
                            ]
                    ],
                    A.Ready_Exp (A.Proc_Exp cloName contExp) ], n2, tbl3)
    where
        mkProc :: Identifier -> [Identifier] -> Identifier -> A.Exp -> A.Exp
        mkProc fvsName ys x body = 
            A.Proc_Exp fvsName 
                (A.LetTuple_Exp ys (A.Var_Exp fvsName) 
                    (A.Proc_Exp x body))
    
comp (R.Call_Exp e1 locb e2) loc senv n tbl k =
    comp e1 loc senv n tbl (\clo n1 tbl1 -> 
        comp e2 loc senv n1 tbl1 (\arg n2 tbl2 ->
            if loc == locb then
                k (A.Call_Exp clo arg) n2 tbl2
            else
                let retName = "ret" ++ show n2 
                    (contExp, n3, tbl3) = k (A.Var_Exp retName) (n2+1) tbl2
                in 
                (A.Block_Exp [
                    A.Send_Exp [
                        A.Var_Exp locb, A.Var_Exp constCALLCLO, 
                            A.Tuple_Exp [
                                clo,
                                arg,
                                A.Var_Exp loc
                            ]
                    ],
                    A.Ready_Exp (A.Proc_Exp retName contExp) ], n3, tbl3) ))
    
comp (R.Const_Exp n) _ _ _ _ _ = undefined
comp (R.Diff_Exp e1 e2) loc senv n tbl k = undefined
comp (R.IsZero_Exp e) loc senv n tbl k = undefined
comp (R.If_Exp e1 e2 e3) loc senv n tbl k = undefined
comp (R.Let_Exp x e1 e2) loc senv n tbl k = undefined

compMain :: R.Exp -> A.Exp
compMain e = 
    let senv = Map.empty
        n = 0
        tbl = Map.empty
        (e1,n1,tbl1) = comp e "main" senv n tbl (,,) -- Wow! (,,) = \e n tbl -> (e, n, tbl)) 
    in e1
        
--
apply_senv :: SEnv -> Identifier -> Identifier
apply_senv senv x = case Map.lookup x senv of
    Just y  -> y
    Nothing -> error ("unbound variable: " ++ x)
