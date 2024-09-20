module Expr where

import Data.Maybe

-- for abstract syntax tree
type Program = Exp
  
data Exp =
    Const_Exp  Int
  | Diff_Exp   Exp Exp
  | IsZero_Exp Exp
  | If_Exp     Exp Exp Exp
  | Var_Exp    Identifier
  | Let_Exp    Identifier Exp Exp
  | Letrec_Exp OptionalType Identifier Identifier OptionalType Exp Exp -- letrec f(x) = ... recusive expr ...
  | Proc_Exp   Identifier OptionalType Exp           -- proc
  | Call_Exp   Exp Exp                       -- call
  deriving Show

type Identifier = String

type TypeVariable = Integer

data Type =
    TyInt
  | TyBool
  | TyFun Type Type
  | TyVar Integer
  deriving (Show, Eq)

data OptionalType = 
    NoType
  | AType Type
  deriving (Show, Eq)

-- for parser
data AST =
    ASTExp { fromASTExp :: Exp }
  | ASTType { fromASTType :: Type }
  | ASTOptionalType { fromASTOptionalType :: OptionalType }
  deriving Show

toASTExp exp = ASTExp exp

toASTType ty = ASTType ty

toASTOptionalType optty = ASTOptionalType optty


-- for testing the type checker
type TestCaseName = String
type ExprText = String

data TypeDeclTestCase = TDTC TestCaseName ExprText (Maybe Type)

data TypeDeclTestSuite = TypeDeclTestSuite [ TypeDeclTestCase ]


