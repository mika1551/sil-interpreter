{-# LANGUAGE BangPatterns #-}

module MachineInterpreter
  ( MachineError (..)
  , validateMachine
  , runMachine
  , runMachineValues
  , runMachineInput
  ) where

import qualified Environment as Env
import Control.Monad (foldM, void)
import Data.Array (Array, bounds, (!), listArray)
import qualified Data.Map.Strict as Map
import Machine (Instruction (..), MachineProgram (..))
import Input (InputValue (..))
import qualified Interpreter as I
import qualified Data.Set as Set

data MachineError
  = DuplicateLabel String
  | UnknownLabel String
  | StackUnderflow Int
  | UndefinedVariable String
  | InputExhausted
  | DivisionByZero
  | TypeError String
  | IndexOutOfBounds Int Int
  | InvalidCharacter Int
  | CyclicOutput
  deriving (Eq, Show)

runMachine :: [Int] -> MachineProgram -> Either MachineError [Int]
runMachine values program = runMachineValues values program >>= traverse numberOutput
  where
    numberOutput (I.OutputNumber n) = Right n
    numberOutput _ = Left (TypeError "integer output expected; use runMachineValues")

runMachineValues :: [Int] -> MachineProgram -> Either MachineError [I.OutputValue]
runMachineValues values = runMachineInput (map InputNumber values)

runMachineInput :: [InputValue] -> MachineProgram -> Either MachineError [I.OutputValue]
runMachineInput values (MachineProgram instructions) = do
  labels <- validatedLabels instructions
  let (inputs, allocated) = I.allocateInputs (I.Runtime Env.empty [] [] Map.empty 0) values
  final <- execute (listArray (0, length instructions - 1) instructions) labels
    allocated { I.input = inputs }
  runtimeResult (traverse (I.resolveOutput final Set.empty) (reverse (I.output final)))

runtimeResult :: Either I.StateError a -> Either MachineError a
runtimeResult = either (Left . convert) Right
  where
    convert err = case err of
      I.UndefinedVariable name -> UndefinedVariable name
      I.InputExhausted -> InputExhausted
      I.DivisionByZero -> DivisionByZero
      I.TypeError message -> TypeError message
      I.IndexOutOfBounds index size -> IndexOutOfBounds index size
      I.InvalidCharacter code -> InvalidCharacter code
      I.CyclicOutput -> CyclicOutput
      I.NoMatchingPattern -> TypeError "pattern matching is supported only by the direct interpreter"

validateMachine :: MachineProgram -> Either MachineError ()
validateMachine (MachineProgram instructions) = void (validatedLabels instructions)

validatedLabels :: [Instruction] -> Either MachineError (Map.Map String Int)
validatedLabels instructions = do
  labels <- indexLabels instructions
  mapM_ (checkTarget labels) instructions
  pure labels

indexLabels :: [Instruction] -> Either MachineError (Map.Map String Int)
indexLabels instructions = foldM addLabel Map.empty (zip [0 ..] instructions)
  where
    addLabel labels (position, DefineLabel name)
      | Map.member name labels = Left (DuplicateLabel name)
      | otherwise = Right (Map.insert name position labels)
    addLabel labels _ = Right labels

checkTarget :: Map.Map String Int -> Instruction -> Either MachineError ()
checkTarget labels instruction = case instruction of
  Jump name -> check name
  JumpIfZero name -> check name
  JumpIfNonZero name -> check name
  _ -> Right ()
  where
    check name
      | Map.member name labels = Right ()
      | otherwise = Left (UnknownLabel name)

execute :: Array Int Instruction -> Map.Map String Int -> I.Runtime -> Either MachineError I.Runtime
execute instructions labels = go 0 []
  where
    end = snd (bounds instructions) + 1
    go !pc !stack !state
      | pc == end = Right state
      | otherwise = case instructions ! pc of
          ReadInput -> case I.input state of
            value : rest -> go (pc + 1) (value : stack) state { I.input = rest }
            [] -> Left InputExhausted
          WriteOutput -> case stack of
            value : rest -> go (pc + 1) rest state { I.output = value : I.output state }
            [] -> Left (StackUnderflow pc)
          LoadVar name -> case Env.lookup name (I.variables state) of
            Just value -> go (pc + 1) (value : stack) state
            Nothing -> Left (UndefinedVariable name)
          StoreVar name -> case stack of
            value : rest -> go (pc + 1) rest state { I.variables = Env.assign name value (I.variables state) }
            [] -> Left (StackUnderflow pc)
          PushConst value -> go (pc + 1) (I.Number value : stack) state
          PushString text ->
            let (value, next) = I.allocate state (I.StringObject text)
            in go (pc + 1) (value : stack) next
          MakeArray count
            | count < 0 -> Left (TypeError "array size must be nonnegative")
            | otherwise ->
                let (items, rest) = splitAt count stack
                in if length items /= count then Left (StackUnderflow pc)
                   else let (value, next) = I.allocate state (I.ArrayObject (reverse items))
                        in go (pc + 1) (value : rest) next
          MakeSExpression name count
            | count < 0 -> Left (TypeError "S-expression arity must be nonnegative")
            | otherwise ->
                let (items, rest) = splitAt count stack
                in if length items /= count then Left (StackUnderflow pc)
                   else let (value, next) = I.allocate state (I.SExpressionObject name (reverse items))
                        in go (pc + 1) (value : rest) next
          LoadIndex -> case stack of
            index : object : rest -> do
              position <- runtimeResult (I.expectNumber index)
              value <- runtimeResult (I.readIndex state object position)
              go (pc + 1) (value : rest) state
            _ -> Left (StackUnderflow pc)
          StoreIndex -> case stack of
            value : index : object : rest -> do
              position <- runtimeResult (I.expectNumber index)
              next <- runtimeResult (I.writeIndex state object position value)
              go (pc + 1) rest next
            _ -> Left (StackUnderflow pc)
          ObjectLength -> case stack of
            value : rest -> do
              object <- runtimeResult (I.lookupObject state value)
              let size = case object of
                    I.ArrayObject items -> length items
                    I.StringObject text -> length text
                    I.SExpressionObject _ items -> length items
              go (pc + 1) (I.Number size : rest) state
            _ -> Left (StackUnderflow pc)
          ApplyBinOp op -> case stack of
            right : left : rest -> do
              (result, next) <- runtimeResult (I.apply state op left right)
              go (pc + 1) (result : rest) next
            _ -> Left (StackUnderflow pc)
          DefineLabel _ -> go (pc + 1) stack state
          Jump name -> go (target name) stack state
          JumpIfZero name -> branch (== 0) name
          JumpIfNonZero name -> branch (/= 0) name
      where
        branch predicate name = case stack of
          value : rest -> do
            number <- runtimeResult (I.expectNumber value)
            go (if predicate number then target name else pc + 1) rest state
          [] -> Left (StackUnderflow pc)
    target name = labels Map.! name + 1
