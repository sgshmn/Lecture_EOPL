module TypeInfer where

import Expr
import TyEnv
import Subst

--
typeInfer :: Exp -> IO (Either String Type)
typeInfer exp = return (Right TyInt) -- return (type_of_program exp )



