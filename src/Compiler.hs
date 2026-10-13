module Compiler (compileProgram) where

import AST
import Machine
import qualified Data.DList as D

compileProgram :: Program -> MachineProgram
compileProgram (Program statements) = MachineProgram (D.toList (fst (compileStatements 0 Nothing statements)))

compileStatements :: Int -> Maybe String -> [Stmt] -> (D.DList Instruction, Int)
compileStatements next continuation statements = case statements of
  [] -> (mempty, next)
  statement : rest ->
    let firstContinuation = if null rest then continuation else Nothing
        (first, afterFirst) = compileStmt next firstContinuation statement
        (remaining, afterRest) = compileStatements afterFirst continuation rest
    in (first <> remaining, afterRest)

compileStmt :: Int -> Maybe String -> Stmt -> (D.DList Instruction, Int)
compileStmt next continuation statement = case statement of
  ReadVar name -> (D.fromList [ReadInput, StoreVar name], next)
  Write expression -> (compileExpr expression <> D.singleton WriteOutput, next)
  Assign name expression -> (compileExpr expression <> D.singleton (StoreVar name), next)
  AssignOp name op expression ->
    (D.singleton (LoadVar name) <> compileExpr expression <> D.fromList [ApplyBinOp op, StoreVar name], next)
  Block statements -> compileStatements next continuation statements
  Skip -> (mempty, next)
  If condition thenBranch elseBranch ->
    let (endLabel, afterEnd, endCode) = case continuation of
          Just name -> (name, next, mempty)
          Nothing -> let name = label "end" next
                     in (name, next + 1, D.singleton (DefineLabel name))
        (thenCode, afterThen) = compileStmt afterEnd (Just endLabel) thenBranch
        (elseCode, afterElse) = maybe (mempty, afterThen)
          (compileStmt afterThen continuation) elseBranch
        elseLabel = label "else" afterElse
        conditionCode = compileExpr condition
    in case elseBranch of
         Nothing ->
           (conditionCode <> D.singleton (JumpIfZero endLabel) <> thenCode <> endCode, afterElse)
         Just _ ->
           (conditionCode <> D.singleton (JumpIfZero elseLabel) <> thenCode
             <> D.fromList [Jump endLabel, DefineLabel elseLabel] <> elseCode <> endCode
           , afterElse + 1)
  While condition body ->
    let bodyLabel = label "while_cody" next
        conditionLabel = label "while_cond" (next + 1)
        (bodyCode, afterBody) = compileStmt (next + 2) (Just conditionLabel) body
    in (D.fromList [Jump conditionLabel, DefineLabel bodyLabel] <> bodyCode
        <> D.singleton (DefineLabel conditionLabel) <> compileExpr condition
        <> D.singleton (JumpIfNonZero bodyLabel), afterBody)
  DoWhile body condition ->
    let bodyLabel = label "while_cody" next
        conditionLabel = label "while_cond" (next + 1)
        (bodyCode, afterBody) = compileStmt (next + 2) (Just conditionLabel) body
    in (D.singleton (DefineLabel bodyLabel) <> bodyCode <> D.singleton (DefineLabel conditionLabel)
        <> compileExpr condition <> D.singleton (JumpIfNonZero bodyLabel), afterBody)
  For initial condition step body ->
    compileStmt next continuation (Block [initial, While condition (Block [body, step])])

compileExpr :: Expr -> D.DList Instruction
compileExpr expression = case expression of
  Var name -> D.singleton (LoadVar name)
  Const value -> D.singleton (PushConst value)
  Bin op left right -> compileExpr left <> compileExpr right <> D.singleton (ApplyBinOp op)

label :: String -> Int -> String
label kind number = "L_" ++ kind ++ "_" ++ show number
