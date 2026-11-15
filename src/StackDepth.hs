module StackDepth
  ( StackDepthInfo (..)
  , StackDepthError (..)
  , analyzeStackDepth
  , machineToStackListing
  ) where

import AST (PatternType (..))
import Data.Array (listArray, (!))
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import Machine
import MachineInterpreter (MachineError, validateMachine)
import Operations (binOpName)

data StackDepthInfo = StackDepthInfo
  { instructionDepths :: [Maybe Int] -- Depth after each instruction; Nothing means unreachable.
  , maximumStackDepth :: Int
  , finalStackDepth :: Maybe Int -- Nothing when no path reaches normal termination.
  } deriving (Eq, Show)

data StackDepthError
  = InvalidMachine MachineError
  | StackDepthUnderflow Int Int Int -- Instruction, required depth, actual depth.
  | InconsistentStackDepth Int Int Int -- Instruction (or exit), first depth, incoming depth.
  | InvalidStackOperand Int String
  deriving (Eq, Show)

-- Propagate depths through the control-flow graph, visiting each instruction
-- once. Revisited nodes must agree, including loop back edges and the exit.
analyzeStackDepth :: MachineProgram -> Either StackDepthError StackDepthInfo
analyzeStackDepth program@(MachineProgram instructions) = do
  either (Left . InvalidMachine) Right (validateMachine program)
  depths <- visit IntMap.empty [(0, 0)]
  let after pc = do
        before <- IntMap.lookup pc depths
        let (_, change) = effect (code ! pc)
        pure (before + change)
      afterDepths = map after [0 .. size - 1]
  pure (StackDepthInfo afterDepths
    (maximum (0 : IntMap.elems depths ++ [d | Just d <- afterDepths]))
    (IntMap.lookup size depths))
  where
    size = length instructions
    code = listArray (0, size - 1) instructions
    labels = Map.fromList [(name, pc) | (pc, DefineLabel name) <- zip [0 ..] instructions]
    visit depths [] = Right depths
    visit depths ((pc, depth) : pending) = case IntMap.lookup pc depths of
      Just known
        | known == depth -> visit depths pending
        | otherwise -> Left (InconsistentStackDepth pc known depth)
      Nothing
        | pc == size -> visit (IntMap.insert pc depth depths) pending
        | otherwise -> do
            let instruction = code ! pc
                (required, change) = effect instruction
            case instruction of
              MakeArray n | n < 0 -> Left (InvalidStackOperand pc "negative array size")
              MakeSExpression _ n | n < 0 -> Left (InvalidStackOperand pc "negative S-expression arity")
              MatchPattern (TagTest _ n) | n < 0 -> Left (InvalidStackOperand pc "negative tag arity")
              _ -> Right ()
            if depth < required
              then Left (StackDepthUnderflow pc required depth)
              else visit (IntMap.insert pc depth depths)
                ([(next, depth + change) | next <- successors pc instruction] ++ pending)
    successors pc instruction = case instruction of
      Jump name -> [labels Map.! name]
      JumpIfZero name -> [labels Map.! name, pc + 1]
      JumpIfNonZero name -> [labels Map.! name, pc + 1]
      MatchFail -> []
      _ -> [pc + 1]

-- Minimum input depth and net change, shared by validation and annotation.
effect :: Instruction -> (Int, Int)
effect instruction = case instruction of
  ReadInput -> (0, 1)
  WriteOutput -> (1, -1)
  LoadVar _ -> (0, 1)
  StoreVar _ -> (1, -1)
  PushConst _ -> (0, 1)
  PushString _ -> (0, 1)
  MakeArray n -> (n, 1 - n)
  MakeSExpression _ n -> (n, 1 - n)
  LoadIndex -> (2, -1)
  StoreIndex -> (3, -3)
  MatchPattern _ -> (1, 0)
  DupTop -> (1, 1)
  DropTop -> (1, -1)
  ObjectLength -> (1, 0)
  ApplyBinOp _ -> (2, -1)
  JumpIfZero _ -> (1, -1)
  JumpIfNonZero _ -> (1, -1)
  PrepareScope -> (0, 0)
  EnterScope -> (0, 0)
  LeaveScope -> (0, 0)
  DefineLabel _ -> (0, 0)
  Jump _ -> (0, 0)
  MatchFail -> (0, 0)

machineToStackListing :: MachineProgram -> Either StackDepthError String
machineToStackListing program@(MachineProgram instructions) = do
  info <- analyzeStackDepth program
  pure (unlines (renderRows (zip instructions (instructionDepths info))))
  where
    pad width value = value ++ replicate (max 1 (width - length value)) ' '
    renderRows [] = []
    renderRows ((DefineLabel name, _) : rest) = case rest of
      (DefineLabel _, _) : _ -> (name ++ ":") : renderRows rest
      [] -> [name ++ ":"]
      (instruction, depth) : remaining -> row (name ++ ":") instruction depth : renderRows remaining
    renderRows ((instruction, depth) : rest) = row "" instruction depth : renderRows rest
    row label instruction depth = pad 32 label ++ pad 32 (renderInstruction instruction)
      ++ "-- " ++ maybe "unreachable" (padDepth . show) depth
    padDepth value = replicate (max 0 (3 - length value)) ' ' ++ value

renderInstruction :: Instruction -> String
renderInstruction instruction = case instruction of
  ReadInput -> "READ"
  WriteOutput -> "WRITE"
  LoadVar name -> "LD " ++ name
  StoreVar name -> "ST " ++ name
  PushConst n -> "CONST " ++ show n
  PushString text -> "STRING " ++ show text
  MakeArray n -> "ARRAY " ++ show n
  MakeSExpression name n -> "SEXP " ++ show name ++ ", " ++ show n
  LoadIndex -> "ELEM"
  StoreIndex -> "SET"
  MatchPattern (TypeTest kind) -> "PATT " ++ case kind of
    IntType -> "#int"
    StringType -> "#string"
    ArrayType -> "#array"
    SExpressionType -> "#sexp"
  MatchPattern (TagTest name n) -> "PATT TAG " ++ show name ++ ", " ++ show n
  DupTop -> "DUP"
  DropTop -> "DROP"
  MatchFail -> "MF"
  ObjectLength -> "LENGTH"
  ApplyBinOp op -> "BINOP " ++ show (binOpName op)
  DefineLabel name -> name ++ ":"
  Jump name -> "JMP " ++ name
  JumpIfZero name -> "JZ " ++ name
  JumpIfNonZero name -> "JNZ " ++ name
  PrepareScope -> "SCOPE"
  EnterScope -> "ENTER"
  LeaveScope -> "LEAVE"
