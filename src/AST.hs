module AST
  ( Program (..)
  , Stmt (..)
  , Expr (..)
  , BinOp (..)
  , Pattern (..)
  , PatternType (..)
  ) where

newtype Program = Program [Stmt]
  deriving (Eq, Show)

data Stmt
  = ReadVar String
  | Write Expr
  | Assign String Expr
  | AssignOp String BinOp Expr
  | SetIndex Expr Expr Expr
  | SetIndexOp Expr Expr BinOp Expr
  | While Expr Stmt
  | DoWhile Stmt Expr
  | For Stmt Expr Stmt Stmt
  | If Expr Stmt (Maybe Stmt)
  | Switch Expr [(Pattern, Stmt)]
  | Block [Stmt]
  | Skip
  
  deriving (Eq, Show)

data Pattern
  = Wildcard
  | BindPattern String Pattern
  | NumberPattern Int
  | StringPattern String
  | ArrayPattern [Pattern]
  | SExpressionPattern String [Pattern]
  | TypePattern PatternType
  deriving (Eq, Show)

data PatternType = ArrayType | IntType | StringType | SExpressionType
  deriving (Eq, Show)

data Expr
  = Var String
  | Const Int
  | Bin BinOp Expr Expr
  | StringLiteral String
  | ArrayLiteral [Expr]
  | SExpression String [Expr]
  | Index Expr Expr
  | Length Expr
  deriving (Eq, Show)

data BinOp
  = Add | Sub | Mul | Div | Mod
  | Eq | Neq | Lt | Le | Gt | Ge
  | And | Or
  deriving (Eq, Show)
