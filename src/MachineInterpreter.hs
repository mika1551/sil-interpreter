{-# LANGUAGE BangPatterns #-}

module MachineInterpreter
  ( MachineError (..)
  , runMachine
  ) where

import Control.Monad (foldM)
import Data.Array (Array, bounds, (!), listArray)
import qualified Data.Map.Strict as Map
import Machine (Instruction (..), MachineProgram (..))
import qualified Operations

data MachineError
  = DuplicateLabel String
  | UnknownLabel String
  | StackUnderflow Int
  | UndefinedVariable String
  | InputExhausted
  | DivisionByZero
  deriving (Eq, Show)

runMachine :: [Int] -> MachineProgram -> Either MachineError [Int]
runMachine values (MachineProgram instructions) = do
  labels <- indexLabels instructions
  mapM_ (checkTarget labels) instructions
  execute (listArray (0, length instructions - 1) instructions) labels values

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

execute :: Array Int Instruction -> Map.Map String Int -> [Int] -> Either MachineError [Int]
execute instructions labels values = go 0 [] Map.empty values []
  where
    end = snd (bounds instructions) + 1

    go !pc !stack !variables !remaining !written
      | pc == end = Right (reverse written)
      | otherwise = case instructions ! pc of
          ReadInput -> case remaining of
            value : rest -> go (pc + 1) (value : stack) variables rest written
            [] -> Left InputExhausted
          WriteOutput -> case stack of
            value : rest -> go (pc + 1) rest variables remaining (value : written)
            [] -> Left (StackUnderflow pc)
          LoadVar name -> case Map.lookup name variables of
            Just value -> go (pc + 1) (value : stack) variables remaining written
            Nothing -> Left (UndefinedVariable name)
          StoreVar name -> case stack of
            value : rest -> go (pc + 1) rest (Map.insert name value variables) remaining written
            [] -> Left (StackUnderflow pc)
          PushConst value -> go (pc + 1) (value : stack) variables remaining written
          ApplyBinOp op -> case stack of
            right : left : rest -> case Operations.applyBinOp op left right of
              Right result -> go (pc + 1) (result : rest) variables remaining written
              Left Operations.DivisionByZero -> Left DivisionByZero
            _ -> Left (StackUnderflow pc)
          DefineLabel _ -> go (pc + 1) stack variables remaining written
          Jump name -> go (target name) stack variables remaining written
          JumpIfZero name -> case stack of
            value : rest -> go (if value == 0 then target name else pc + 1) rest variables remaining written
            [] -> Left (StackUnderflow pc)
          JumpIfNonZero name -> case stack of
            value : rest -> go (if value /= 0 then target name else pc + 1) rest variables remaining written
            [] -> Left (StackUnderflow pc)

    target name = labels Map.! name + 1
