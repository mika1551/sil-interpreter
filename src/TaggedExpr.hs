module TaggedExpr
  ( decodeExpr
  , decodeState
  , evalExpr
  , evalCompiled
  , runTagged
  ) where

import AST (Expr (..), Program (..), Stmt (..))
import Compiler (compileExpr)
import Data.Aeson (Value (..), eitherDecodeStrictText)
import Data.Foldable (toList)
import Data.Scientific (Scientific, toBoundedInteger)
import qualified Data.Text as Text
import Interpreter (StateError (..), runProgram)
import Machine (Instruction (..), MachineProgram (..))
import qualified MachineInterpreter as Machine
import Operations (parseBinOpName)

decodeExpr :: Value -> Either String Expr
decodeExpr (Array items) = case toList items of
  [Number tag, number] | isTag 0 tag -> Const <$> integerAt number
  [Number tag, String name] | isTag 1 tag -> Right (Var (Text.unpack name))
  [Number tag, String name, left, right] | isTag 2 tag -> do
    op <- maybe (Left ("unknown operator: " ++ Text.unpack name)) Right
      (parseBinOpName (Text.unpack name))
    Bin op <$> decodeExpr left <*> decodeExpr right
  _ -> Left "invalid tagged expression"
decodeExpr _ = Left "expression must be a JSON array"

decodeState :: Value -> Either String [(String, Int)]
decodeState (Array items) = case toList items of
  [Number tag] | isTag 0 tag -> Right []
  [Number tag, Array pair, rest] | isTag 1 tag -> case toList pair of
    [String name, number] -> do
      value <- integerAt number
      remaining <- decodeState rest
      Right ((Text.unpack name, value) : remaining)
    _ -> Left "state entry must be [name, number]"
  _ -> Left "invalid tagged state"
decodeState _ = Left "state must be a JSON array"

integerAt :: Value -> Either String Int
integerAt (Number number) =
  maybe (Left "expected an Int") Right (toBoundedInteger number)
integerAt _ = Left "expected an Int"

isTag :: Int -> Scientific -> Bool
isTag expected actual = toBoundedInteger actual == Just expected

evalExpr :: [(String, Int)] -> Expr -> Either StateError Int
evalExpr state expression = do
  values <- runProgram [] (Program (assignments state ++ [Write expression]))
  case values of
    [value] -> Right value
    _ -> Left (TypeError "expected one expression result")

evalCompiled :: [(String, Int)] -> Expr -> Either String Int
evalCompiled _ expression | not (numericExpr expression) =
  Left "only integer expressions are supported"
evalCompiled state expression = do
  values <- either (Left . show) Right
    (Machine.runMachine [] (MachineProgram instructions))
  case values of
    [value] -> Right value
    _ -> Left "expected one expression result"
  where
    instructions = concatMap initialize (reverse state)
      ++ compileExpr expression ++ [WriteOutput]
    initialize (name, value) = [PushConst value, StoreVar name]

numericExpr :: Expr -> Bool
numericExpr expression = case expression of
  Const _ -> True
  Var _ -> True
  Bin _ left right -> numericExpr left && numericExpr right
  _ -> False

assignments :: [(String, Int)] -> [Stmt]
assignments state = [Assign name (Const value) | (name, value) <- reverse state]

runTagged :: String -> String -> Either String (Int, [Instruction], Int)
runTagged expressionSource stateSource = do
  expressionJson <- eitherDecodeStrictText (Text.pack expressionSource)
  stateJson <- eitherDecodeStrictText (Text.pack stateSource)
  expression <- decodeExpr expressionJson
  state <- decodeState stateJson
  direct <- either (Left . show) Right (evalExpr state expression)
  compiled <- evalCompiled state expression
  Right (direct, compileExpr expression, compiled)
