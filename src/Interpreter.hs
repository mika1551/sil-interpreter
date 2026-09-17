module Interpreter
  ( Runtime (..)
  , StateError (..)
  , runProgram
  ) where

import AST
import qualified Data.Map.Strict as Map

data Runtime = Runtime
  { variables :: Map.Map String Int
  , input :: [Int]
  , output :: [Int]
  } deriving (Eq, Show)

data StateError
  = UndefinedVariable String
  | InputExhausted
  | DivisionByZero
  deriving (Eq, Show)

runProgram :: [Int] -> Program -> Either StateError [Int]
runProgram values (Program statements) = do
  finalState <- execute (Runtime Map.empty values []) (Block statements)
  pure (reverse (output finalState))

execute :: Runtime -> Stmt -> Either StateError Runtime
execute state statement = case statement of
  ReadVar name -> case input state of
    [] -> Left InputExhausted
    value : rest -> Right state { variables = Map.insert name value (variables state), input = rest }
  Write expression -> do
    value <- evaluate state expression
    Right state { output = value : output state }
  Assign name expression -> do
    value <- evaluate state expression
    Right state { variables = Map.insert name value (variables state) }
  AssignOp name op expression -> do
    oldValue <- lookupVar state name
    rightValue <- evaluate state expression
    value <- apply op oldValue rightValue
    Right state { variables = Map.insert name value (variables state) }
  While condition body -> loop state
    where
      loop current = do
        value <- evaluate current condition
        if truthy value then execute current body >>= loop else Right current
  DoWhile body condition -> do
    afterBody <- execute state body
    execute afterBody (While condition body)
  For initial condition step body -> do
    afterInitial <- execute state initial
    execute afterInitial (While condition (Block [body, step]))
  If condition thenBranch elseBranch -> do
    value <- evaluate state condition
    if truthy value then execute state thenBranch else maybe (Right state) (execute state) elseBranch
  Block statements -> foldl continue (Right state) statements
  Skip -> Right state
  where
    continue current next = current >>= (`execute` next)

evaluate :: Runtime -> Expr -> Either StateError Int
evaluate state expression = case expression of
  Var name -> lookupVar state name
  Const value -> Right value
  Bin op left right -> do
    leftValue <- evaluate state left
    rightValue <- evaluate state right
    apply op leftValue rightValue

lookupVar :: Runtime -> String -> Either StateError Int
lookupVar state name = maybe (Left (UndefinedVariable name)) Right (Map.lookup name (variables state))

truthy :: Int -> Bool
truthy value = value /= 0

apply :: BinOp -> Int -> Int -> Either StateError Int
apply op left right = case op of
  Add -> Right (left + right); Sub -> Right (left - right); Mul -> Right (left * right)
  Div -> if right == 0 then Left DivisionByZero else Right (left `div` right)
  Mod -> if right == 0 then Left DivisionByZero else Right (left `mod` right)
  Eq -> boolean (left == right); Neq -> boolean (left /= right)
  Lt -> boolean (left < right); Le -> boolean (left <= right)
  Gt -> boolean (left > right); Ge -> boolean (left >= right)
  And -> boolean (truthy left && truthy right)
  Or -> boolean (truthy left || truthy right)
  where
    boolean condition = Right (if condition then 1 else 0)
