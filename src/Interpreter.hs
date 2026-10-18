module Interpreter
  ( Runtime (..)
  , Value (..)
  , HeapObject (..)
  , OutputValue (..)
  , StateError (..)
  , runProgram
  , runProgramValues
  ) where

import AST
import Control.Monad (foldM)
import Data.Char (chr, ord)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Operations

data Value
  = Number Int
  | Ref Int
  deriving (Eq, Show)

data HeapObject
  = ArrayObject [Value]
  | StringObject String
  deriving (Eq, Show)

data OutputValue
  = OutputNumber Int
  | OutputString String
  | OutputArray [OutputValue]
  deriving (Eq, Show)

data Runtime = Runtime
  { variables :: Map.Map String Value
  , input :: [Int]
  , output :: [Value]
  , heap :: Map.Map Int HeapObject
  , nextAddress :: Int
  } deriving (Eq, Show)

data StateError
  = UndefinedVariable String
  | InputExhausted
  | DivisionByZero
  | TypeError String
  | IndexOutOfBounds Int Int
  | InvalidCharacter Int
  | CyclicOutput
  deriving (Eq, Show)

runProgram :: [Int] -> Program -> Either StateError [Int]
runProgram values program = runProgramValues values program >>= traverse numberOutput
  where
    numberOutput (OutputNumber n) = Right n
    numberOutput _ = Left (TypeError "integer output expected; use runProgramValues")

runProgramValues :: [Int] -> Program -> Either StateError [OutputValue]
runProgramValues values (Program statements) = do
  let initial = Runtime Map.empty values [] Map.empty 0
  finalState <- execute initial (Block statements)
  traverse (resolveOutput finalState Set.empty) (reverse (output finalState))

execute :: Runtime -> Stmt -> Either StateError Runtime
execute state statement = case statement of
  ReadVar name -> case input state of
    [] -> Left InputExhausted
    value : rest -> Right state { variables = Map.insert name (Number value) (variables state), input = rest }
  Write expression -> do
    (value, next) <- evaluate state expression
    Right next { output = value : output next }
  Assign name expression -> do
    (value, next) <- evaluate state expression
    Right next { variables = Map.insert name value (variables next) }
  AssignOp name op expression -> do
    oldValue <- lookupVar state name
    (rightValue, next) <- evaluate state expression
    (value, final) <- apply next op oldValue rightValue
    Right final { variables = Map.insert name value (variables final) }
  SetIndex target index expression -> do
    (object, afterTarget) <- evaluate state target
    (indexValue, afterIndex) <- evaluate afterTarget index
    position <- expectNumber indexValue
    (value, afterValue) <- evaluate afterIndex expression
    writeIndex afterValue object position value
  SetIndexOp target index op expression -> do
    (object, afterTarget) <- evaluate state target
    (indexValue, afterIndex) <- evaluate afterTarget index
    position <- expectNumber indexValue
    oldValue <- readIndex afterIndex object position
    (rightValue, afterValue) <- evaluate afterIndex expression
    (value, final) <- apply afterValue op oldValue rightValue
    writeIndex final object position value
  While condition body -> loop state
    where
      loop current = do
        (value, next) <- evaluate current condition
        conditionValue <- expectNumber value
        if conditionValue /= 0 then execute next body >>= loop else Right next
  DoWhile body condition -> do
    afterBody <- execute state body
    execute afterBody (While condition body)
  For initial condition step body -> do
    afterInitial <- execute state initial
    execute afterInitial (While condition (Block [body, step]))
  If condition thenBranch elseBranch -> do
    (value, next) <- evaluate state condition
    conditionValue <- expectNumber value
    if conditionValue /= 0 then execute next thenBranch else maybe (Right next) (execute next) elseBranch
  Block statements -> foldM execute state statements
  Skip -> Right state

evaluate :: Runtime -> Expr -> Either StateError (Value, Runtime)
evaluate state expression = case expression of
  Var name -> do
    value <- lookupVar state name
    Right (value, state)
  Const value -> Right (Number value, state)
  Bin op left right -> do
    (leftValue, afterLeft) <- evaluate state left
    (rightValue, afterRight) <- evaluate afterLeft right
    apply afterRight op leftValue rightValue
  StringLiteral text -> Right (allocate state (StringObject text))
  ArrayLiteral expressions -> do
    (reversed, next) <- foldM evalElement ([], state) expressions
    Right (allocate next (ArrayObject (reverse reversed)))
    where
      evalElement (values, current) item = do
        (value, next) <- evaluate current item
        Right (value : values, next)
  Index target index -> do
    (object, afterTarget) <- evaluate state target
    (indexValue, afterIndex) <- evaluate afterTarget index
    position <- expectNumber indexValue
    value <- readIndex afterIndex object position
    Right (value, afterIndex)
  Length target -> do
    (object, next) <- evaluate state target
    stored <- lookupObject next object
    let size = case stored of
          ArrayObject values -> length values
          StringObject text -> length text
    Right (Number size, next)

allocate :: Runtime -> HeapObject -> (Value, Runtime)
allocate state object =
  let address = nextAddress state
  in (Ref address, state { heap = Map.insert address object (heap state), nextAddress = address + 1 })

lookupVar :: Runtime -> String -> Either StateError Value
lookupVar state name = maybe (Left (UndefinedVariable name)) Right (Map.lookup name (variables state))

lookupObject :: Runtime -> Value -> Either StateError HeapObject
lookupObject state (Ref address) =
  maybe (Left (TypeError "invalid object reference")) Right (Map.lookup address (heap state))
lookupObject _ (Number _) = Left (TypeError "array or string expected")

expectNumber :: Value -> Either StateError Int
expectNumber (Number value) = Right value
expectNumber _ = Left (TypeError "integer expected")

readIndex :: Runtime -> Value -> Int -> Either StateError Value
readIndex state reference position = do
  object <- lookupObject state reference
  case object of
    ArrayObject values -> elementAt position values
    StringObject text -> Number . ord <$> elementAt position text

writeIndex :: Runtime -> Value -> Int -> Value -> Either StateError Runtime
writeIndex state reference position value = do
  object <- lookupObject state reference
  updated <- case object of
    ArrayObject values -> ArrayObject <$> replaceAt position value values
    StringObject text -> do
      code <- expectNumber value
      if code < 0 || code > 0x10ffff || (code >= 0xd800 && code <= 0xdfff)
        then Left (InvalidCharacter code)
        else StringObject <$> replaceAt position (chr code) text
  case reference of
    Ref address -> Right state { heap = Map.insert address updated (heap state) }
    Number _ -> Left (TypeError "array or string expected")

elementAt :: Int -> [a] -> Either StateError a
elementAt position values
  | position < 0 || position >= length values = Left (IndexOutOfBounds position (length values))
  | otherwise = Right (values !! position)

replaceAt :: Int -> a -> [a] -> Either StateError [a]
replaceAt position value values
  | position < 0 || position >= length values = Left (IndexOutOfBounds position (length values))
  | otherwise = Right (take position values ++ [value] ++ drop (position + 1) values)

apply :: Runtime -> BinOp -> Value -> Value -> Either StateError (Value, Runtime)
apply state Add left@(Ref _) right@(Ref _) = do
  leftObject <- lookupObject state left
  rightObject <- lookupObject state right
  case (leftObject, rightObject) of
    (ArrayObject leftValues, ArrayObject rightValues) ->
      Right (allocate state (ArrayObject (leftValues ++ rightValues)))
    _ -> Left (TypeError "addition expects two integers or two arrays")
apply state op left right = do
  leftNumber <- expectNumber left
  rightNumber <- expectNumber right
  case Operations.applyBinOp op leftNumber rightNumber of
    Left Operations.DivisionByZero -> Left DivisionByZero
    Right value -> Right (Number value, state)

resolveOutput :: Runtime -> Set.Set Int -> Value -> Either StateError OutputValue
resolveOutput _ _ (Number value) = Right (OutputNumber value)
resolveOutput state visited (Ref address)
  | Set.member address visited = Left CyclicOutput
  | otherwise = do
      object <- lookupObject state (Ref address)
      case object of
        StringObject text -> Right (OutputString text)
        ArrayObject values ->
          OutputArray <$> traverse (resolveOutput state (Set.insert address visited)) values