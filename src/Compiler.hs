module Compiler (compileProgram) where

import AST
import Machine

compileProgram :: Program -> MachineProgram
compileProgram (Program statements) = MachineProgram (fst (compileStatements 0 Nothing statements))

compileStatements :: Int -> Maybe String -> [Stmt] -> ([Instruction], Int)
compileStatements next continuation statements = case statements of
  [] -> ([], next)
  statement : rest ->
    let firstContinuation = if null rest then continuation else Nothing
        (first, afterFirst) = compileStmt next firstContinuation statement
        (remaining, afterRest) = compileStatements afterFirst continuation rest
    in (first ++ remaining, afterRest)

compileStmt :: Int -> Maybe String -> Stmt -> ([Instruction], Int)
compileStmt next continuation statement = case statement of
  ReadVar name -> ([ReadInput, StoreVar name], next)
  Write expression -> (compileExpr expression ++ [WriteOutput], next)
  Assign name expression -> (compileExpr expression ++ [StoreVar name], next)
  AssignOp name op expression ->
    (LoadVar name : compileExpr expression ++ [ApplyBinOp op, StoreVar name], next)
  Block statements -> compileStatements next continuation statements
  Skip -> ([], next)
  If condition thenBranch elseBranch ->
    let (endLabel, afterEnd, endCode) = case continuation of
          Just name -> (name, next, [])
          Nothing -> let name = label "end" next
                     in (name, next + 1, [DefineLabel name])
        (thenCode, afterThen) = compileStmt afterEnd (Just endLabel) thenBranch
        (elseCode, afterElse) = maybe ([], afterThen)
          (compileStmt afterThen continuation) elseBranch
        elseLabel = label "else" afterElse
        conditionCode = compileExpr condition
    in case (null thenCode, null elseCode) of
         (_, True) ->
           (conditionCode ++ [JumpIfZero endLabel] ++ thenCode ++ endCode, afterElse)
         (True, False) ->
           (conditionCode ++ [JumpIfNonZero endLabel] ++ elseCode ++ endCode, afterElse)
         (False, False) ->
           (conditionCode ++ [JumpIfZero elseLabel] ++ thenCode
             ++ [Jump endLabel, DefineLabel elseLabel] ++ elseCode ++ endCode
           , afterElse + 1)
  While condition body ->
    let bodyLabel = label "while_cody" next
        conditionLabel = label "while_cond" (next + 1)
        (bodyCode, afterBody) = compileStmt (next + 2) (Just conditionLabel) body
    in ([Jump conditionLabel, DefineLabel bodyLabel] ++ bodyCode
        ++ [DefineLabel conditionLabel] ++ compileExpr condition
        ++ [JumpIfNonZero bodyLabel], afterBody)
  DoWhile body condition ->
    let bodyLabel = label "while_cody" next
        conditionLabel = label "while_cond" (next + 1)
        (bodyCode, afterBody) = compileStmt (next + 2) (Just conditionLabel) body
    in ([DefineLabel bodyLabel] ++ bodyCode ++ [DefineLabel conditionLabel]
        ++ compileExpr condition ++ [JumpIfNonZero bodyLabel], afterBody)
  For initial condition step body ->
    compileStmt next continuation (Block [initial, While condition (Block [body, step])])

compileExpr :: Expr -> [Instruction]
compileExpr expression = case expression of
  Var name -> [LoadVar name]
  Const value -> [PushConst value]
  Bin op left right -> compileExpr left ++ compileExpr right ++ [ApplyBinOp op]

label :: String -> Int -> String
label kind number = "L_" ++ kind ++ "_" ++ show number
