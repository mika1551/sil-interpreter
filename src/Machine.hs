module Machine
  ( MachineProgram (..)
  , Instruction (..)
  ) where

import AST (BinOp)

newtype MachineProgram = MachineProgram [Instruction]
  deriving (Eq, Show)

data Instruction
  = ReadInput
  | WriteOutput
  | LoadVar String
  | StoreVar String
  | PushConst Int
  | PushString String
  | MakeArray Int
  | MakeSExpression String Int
  | LoadIndex
  | StoreIndex
  | ObjectLength
  | ApplyBinOp BinOp
  | DefineLabel String
  | Jump String
  | JumpIfZero String
  | JumpIfNonZero String
  deriving (Eq, Show)
