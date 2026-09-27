{-# LANGUAGE OverloadedStrings #-}

module MachineParser
  ( parseMachine
  ) where

import Data.Aeson (Value (..), eitherDecodeStrictText)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Foldable (toList)
import Data.Scientific (floatingOrInteger)
import qualified Data.Text as Text
import JsonValidation (validateJsonSyntax)
import Machine (Instruction (..), MachineProgram (..))
import Operations (parseBinOpName)

parseMachine :: String -> Either String MachineProgram
parseMachine source = do
  let input = Text.pack source
  value <- eitherDecodeStrictText input
  validateJsonSyntax input
  case value of
    Array instructions -> MachineProgram <$> traverse parseAt (zip [0 :: Int ..] (toList instructions))
    _ -> Left "$: expected an array of instructions"

parseAt :: (Int, Value) -> Either String Instruction
parseAt (index, value) = case value of
  String "READ" -> Right ReadInput
  String "WRITE" -> Right WriteOutput
  String name -> Left (path ++ ": unknown instruction " ++ show (Text.unpack name))
  Object fields -> case KeyMap.toList fields of
    [(key, operand)] -> parseObject (path ++ "." ++ Key.toString key) (Key.toString key) operand
    _ -> Left (path ++ ": expected exactly one instruction field")
  _ -> Left (path ++ ": expected an instruction")
  where
    path = "$[" ++ show index ++ "]"

parseObject :: String -> String -> Value -> Either String Instruction
parseObject path name operand = case name of
  "LD" -> LoadVar <$> stringAt path operand
  "ST" -> StoreVar <$> stringAt path operand
  "CONST" -> PushConst <$> intAt path operand
  "BINOP" -> do
    operator <- stringAt path operand
    case parseBinOpName operator of
      Just op -> Right (ApplyBinOp op)
      Nothing -> Left (path ++ ": unknown operator " ++ show operator)
  "LABEL" -> DefineLabel <$> stringAt path operand
  "JMP" -> Jump <$> stringAt path operand
  "JZ" -> JumpIfZero <$> stringAt path operand
  "JNZ" -> JumpIfNonZero <$> stringAt path operand
  _ -> Left (path ++ ": unknown instruction " ++ show name)

stringAt :: String -> Value -> Either String String
stringAt _ (String value) = Right (Text.unpack value)
stringAt path _ = Left (path ++ ": expected a string")

intAt :: String -> Value -> Either String Int
intAt path (Number value) = case floatingOrInteger value :: Either Double Integer of
  Right integer
    | integer >= toInteger (minBound :: Int) && integer <= toInteger (maxBound :: Int) -> Right (fromInteger integer)
    | otherwise -> Left (path ++ ": integer is outside the supported Int range")
  Left _ -> Left (path ++ ": expected an integer")
intAt path _ = Left (path ++ ": expected an integer")
