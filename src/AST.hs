module AST
  ( Program (..)
  , Stmt (..)
  , Expr (..)
  , BinOp (..)
  ) where

data Program = Program [Stmt]
  deriving (Eq, Show)

data Stmt
  = ReadVar String
  | Write Expr
  | Assign String Expr
  | AssignOp String BinOp Expr
  | While Expr Stmt
  | DoWhile Stmt Expr
  | For Stmt Expr Stmt Stmt
  | If Expr Stmt (Maybe Stmt)
  | Block [Stmt]
  | Skip
  deriving (Eq, Show)

data Expr
  = Var String
  | Const Int
  | Bin BinOp Expr Expr
  deriving (Eq, Show)

data BinOp
  = Add | Sub | Mul | Div | Mod
  | Eq | Neq | Lt | Le | Gt | Ge
  | And | Or
  deriving (Eq, Show)
