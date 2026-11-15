module Machine
  ( MachineProgram (..)
  , Instruction (..)
  , PatternTest (..)
  ) where

import AST (BinOp, PatternType)

newtype MachineProgram = MachineProgram [Instruction]
  deriving (Eq, Show)

data Instruction
  = ReadInput
  | WriteOutput
  | LoadVar String
  | StoreVar String
  | PrepareScope
  | EnterScope
  | LeaveScope
  | PushConst Int
  | PushString String
  | MakeArray Int
  | MakeSExpression String Int
  | LoadIndex
  | StoreIndex
  | MatchPattern PatternTest
  | DupTop
  | DropTop
  | MatchFail
  | ObjectLength
  | ApplyBinOp BinOp
  | DefineLabel String
  | Jump String
  | JumpIfZero String
  | JumpIfNonZero String
  deriving (Eq, Show)

data PatternTest
  = TypeTest PatternType
  | TagTest String Int
  deriving (Eq, Show)
