module Expr where

import Data.Maybe

-- for abstract syntax tree
data Program = Program [ ModuleDef ] Exp 
  deriving Show

data ModuleDef = ModuleDef Identifier Interface ModuleBody
  deriving Show

data ModuleBody = ModuleBody [ Definition ] 
  deriving Show

data Interface = SimpleIface [ Declaration ]
  deriving Show

data Declaration = ValDecl Identifier Type 
  deriving Show

data Definition = ValDefn Identifier Exp 
  deriving Show
  
data Exp =
    Const_Exp  Int
  | Diff_Exp   Exp Exp
  | IsZero_Exp Exp
  | If_Exp     Exp Exp Exp
  | Var_Exp    Identifier
  | Let_Exp    Identifier Exp Exp
  | Letrec_Exp Type Identifier Identifier Type Exp Exp -- letrec f(x) = ... recusive expr ...
  | Proc_Exp   Identifier Type Exp           -- proc
  | Call_Exp   Exp Exp                       -- call
  | QualifiedVar_Exp Identifier Identifier
  deriving Show

type Identifier = String

data Type =
    TyInt
  | TyBool
  | TyFun Type Type
  | TyName Identifier
  | TyQualified Identifier Identifier
  deriving (Show, Eq)


-- for parser
data AST =
    ASTExp { fromASTExp :: Exp }
  | ASTType { fromASTType :: Type }
  | ASTProgram { fromASTProgram :: Program }
  | ASTModuleDef { fromASTModuleDef :: ModuleDef }
  | ASTModuleBody { fromASTModuleBody :: ModuleBody }
  | ASTInterface { fromASTInterface :: Interface }
  | ASTDeclaration { fromASTDeclaration :: Declaration }
  | ASTDefinition { fromASTDefinition :: Definition }
  deriving Show

toASTExp exp = ASTExp exp

toASTType ty = ASTType ty


-- for testing the type checker
type TestCaseName = String
type ExprText = String

data TypeDeclTestCase = TDTC TestCaseName ExprText (Maybe Type)

data TypeDeclTestSuite = TypeDeclTestSuite [ TypeDeclTestCase ]


