module Compiler (compileProgram) where

import AST
import Machine

compileProgram :: Program -> MachineProgram
compileProgram (Program statements) = MachineProgram (fst (compileStatements 0 statements))

compileStatements :: Int -> [Stmt] -> ([Instruction], Int)
compileStatements next statements = case statements of
  [] -> ([], next)
  statement : rest ->
    let (first, afterFirst) = compileStmt next statement
        (remaining, afterRest) = compileStatements afterFirst rest
    in (first ++ remaining, afterRest)

compileStmt :: Int -> Stmt -> ([Instruction], Int)
compileStmt next statement = case statement of
  ReadVar name -> ([ReadInput, StoreVar name], next)
  Write expression -> (compileExpr expression ++ [WriteOutput], next)
  Assign name expression -> (compileExpr expression ++ [StoreVar name], next)
  AssignOp name op expression ->
    (LoadVar name : compileExpr expression ++ [ApplyBinOp op, StoreVar name], next)
  Block statements -> compileStatements next statements
  Skip -> ([], next)
  If condition thenBranch elseBranch ->
    let elseLabel = label "else" next
        endLabel = label "end" (next + 1)
        (thenCode, afterThen) = compileStmt (next + 2) thenBranch
        (elseCode, afterElse) = maybe ([], afterThen) (compileStmt afterThen) elseBranch
    in (compileExpr condition ++ [JumpIfZero elseLabel] ++ thenCode
        ++ [Jump endLabel, DefineLabel elseLabel] ++ elseCode
        ++ [DefineLabel endLabel], afterElse)
  While condition body ->
    let bodyLabel = label "while_cody" next
        conditionLabel = label "while_cond" (next + 1)
        (bodyCode, afterBody) = compileStmt (next + 2) body
    in ([Jump conditionLabel, DefineLabel bodyLabel] ++ bodyCode
        ++ [DefineLabel conditionLabel] ++ compileExpr condition
        ++ [JumpIfNonZero bodyLabel], afterBody)
  DoWhile body condition ->
    let bodyLabel = label "while_cody" next
        conditionLabel = label "while_cond" (next + 1)
        (bodyCode, afterBody) = compileStmt (next + 2) body
    in ([DefineLabel bodyLabel] ++ bodyCode ++ [DefineLabel conditionLabel]
        ++ compileExpr condition ++ [JumpIfNonZero bodyLabel], afterBody)
  For initial condition step body ->
    compileStmt next (Block [initial, While condition (Block [body, step])])

compileExpr :: Expr -> [Instruction]
compileExpr expression = case expression of
  Var name -> [LoadVar name]
  Const value -> [PushConst value]
  Bin op left right -> compileExpr left ++ compileExpr right ++ [ApplyBinOp op]

label :: String -> Int -> String
label kind number = "L_" ++ kind ++ "_" ++ show number
