module Machine
  ( MachineProgram (..)
  , Instruction (..)
  ) where

import AST (BinOp, PatternType)

newtype MachineProgram = MachineProgram [Instruction]
  deriving (Eq, Show)

data Instruction
  = ReadInput
  | WriteOutput
  | LoadVar String
  | StoreVar String
  | BindVar String
  | EnterScope
  | LeaveScope
  | PushConst Int
  | PushString String
  | MakeArray Int
  | MakeSExpression String Int
  | LoadIndex
  | StoreIndex
  | IsType PatternType
  | IsTag String Int
  | StringEquals String
  | MatchFail
  | ObjectLength
  | ApplyBinOp BinOp
  | DefineLabel String
  | Jump String
  | JumpIfZero String
  | JumpIfNonZero String
  deriving (Eq, Show)
