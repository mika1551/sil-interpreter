{-# LANGUAGE OverloadedStrings #-}

module MachineParser
  ( parseMachine
  ) where

import AST (PatternType (..))
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
  String "SCOPE" -> Right EnterScope
  String "ENDSCOPE" -> Right LeaveScope
  String "MATCHFAIL" -> Right MatchFail
  String "READ" -> Right ReadInput
  String "WRITE" -> Right WriteOutput
  String "ELEM" -> Right LoadIndex
  String "SET" -> Right StoreIndex
  String "LENGTH" -> Right ObjectLength
  String name -> Left (path ++ ": unknown instruction " ++ show (Text.unpack name))
  Object fields -> case KeyMap.toList fields of
    [(key, operand)] -> parseObject (path ++ "." ++ Key.toString key) (Key.toString key) operand
    _ -> Left (path ++ ": expected exactly one instruction field")
  _ -> Left (path ++ ": expected an instruction")
  where
    path = "$[" ++ show index ++ "]"

parseObject :: String -> String -> Value -> Either String Instruction
parseObject path name operand = case name of
  "ISTYPE" -> do
    kind <- stringAt path operand
    case kind of
      "int" -> Right (IsType IntType)
      "array" -> Right (IsType ArrayType)
      "string" -> Right (IsType StringType)
      "sexp" -> Right (IsType SExpressionType)
      _ -> Left (path ++ ": unknown value type " ++ show kind)
  "STREQ" -> StringEquals <$> stringAt path operand
  "ISTAG" -> case operand of
    Array values -> case toList values of
      [tag, arity] -> do
        constructor <- stringAt (path ++ "[0]") tag
        count <- intAt (path ++ "[1]") arity
        if count < 0 then Left (path ++ "[1]: S-expression arity must be nonnegative")
          else Right (IsTag constructor count)
      _ -> Left (path ++ ": expected [name, arity]")
    _ -> Left (path ++ ": expected [name, arity]")
  "LD" -> LoadVar <$> stringAt path operand
  "BIND" -> BindVar <$> stringAt path operand
  "ST" -> StoreVar <$> stringAt path operand
  "CONST" -> PushConst <$> intAt path operand
  "STRING" -> PushString <$> stringAt path operand
  "ARRAY" -> do
    count <- intAt path operand
    if count < 0 then Left (path ++ ": array size must be nonnegative") else Right (MakeArray count)
  "SEXP" -> case operand of
    Array values -> case toList values of
      [tag, arity] -> do
        constructor <- stringAt (path ++ "[0]") tag
        count <- intAt (path ++ "[1]") arity
        if count < 0 then Left (path ++ "[1]: S-expression arity must be nonnegative")
          else Right (MakeSExpression constructor count)
      _ -> Left (path ++ ": expected [name, arity]")
    _ -> Left (path ++ ": expected [name, arity]")
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
