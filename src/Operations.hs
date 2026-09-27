module Operations
  ( OperationError (..)
  , applyBinOp
  ) where

import AST (BinOp (..))

data OperationError = DivisionByZero
  deriving (Eq, Show)

applyBinOp :: BinOp -> Int -> Int -> Either OperationError Int
applyBinOp op left right = case op of
  Add -> Right (left + right)
  Sub -> Right (left - right)
  Mul -> Right (left * right)
  Div -> if right == 0 then Left DivisionByZero else Right (left `div` right)
  Mod -> if right == 0 then Left DivisionByZero else Right (left `mod` right)
  Eq -> boolean (left == right)
  Neq -> boolean (left /= right)
  Lt -> boolean (left < right)
  Le -> boolean (left <= right)
  Gt -> boolean (left > right)
  Ge -> boolean (left >= right)
  And -> boolean (truthy left && truthy right)
  Or -> boolean (truthy left || truthy right)
  where
    boolean condition = Right (if condition then 1 else 0)
    truthy value = value /= 0
