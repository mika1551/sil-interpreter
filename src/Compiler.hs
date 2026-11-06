module Compiler (compileProgram, compileExpr) where

import AST
import Data.List (isPrefixOf)
import Machine

compileProgram :: Program -> MachineProgram
compileProgram (Program statements) = MachineProgram (fst (compileStatements prefix 0 Nothing statements))
  where
    names = concatMap statementNames statements
    prefix = head [replicate n '$' | n <- [1 ..], (not . any (replicate n '$' `isPrefixOf`)) names]

compileStatements :: String -> Int -> Maybe String -> [Stmt] -> ([Instruction], Int)
compileStatements prefix next continuation statements = case statements of
  [] -> ([], next)
  statement : rest ->
    let firstContinuation = if null rest then continuation else Nothing
        (first, afterFirst) = compileStmt prefix next firstContinuation statement
        (remaining, afterRest) = compileStatements prefix afterFirst continuation rest
    in (first ++ remaining, afterRest)

compileStmt :: String -> Int -> Maybe String -> Stmt -> ([Instruction], Int)
compileStmt prefix next continuation statement = case statement of
  ReadVar name -> ([ReadInput, StoreVar name], next)
  Write expression -> (compileExpr expression ++ [WriteOutput], next)
  Assign name expression -> (compileExpr expression ++ [StoreVar name], next)
  AssignOp name op expression ->
    (LoadVar name : compileExpr expression ++ [ApplyBinOp op, StoreVar name], next)
  SetIndex target index value ->
    (compileExpr target ++ compileExpr index ++ [PushConst 0, ApplyBinOp Add]
      ++ compileExpr value ++ [StoreIndex], next)
  SetIndexOp target index op value ->
    let objectTemp = prefix ++ "object_" ++ show next
        indexTemp = prefix ++ "index_" ++ show next
    in (compileExpr target ++ [StoreVar objectTemp]
        ++ compileExpr index ++ [StoreVar indexTemp]
        ++ [LoadVar objectTemp, LoadVar indexTemp, LoadVar objectTemp, LoadVar indexTemp, LoadIndex]
        ++ compileExpr value ++ [ApplyBinOp op, StoreIndex], next + 1)
  Block statements -> compileStatements prefix next continuation statements
  Skip -> ([], next)
  If condition thenBranch elseBranch ->
    let (endLabel, afterEnd, endCode) = case continuation of
          Just name -> (name, next, [])
          Nothing -> let name = label "end" next
                     in (name, next + 1, [DefineLabel name])
        (thenCode, afterThen) = compileStmt prefix afterEnd (Just endLabel) thenBranch
        (elseCode, afterElse) = maybe ([], afterThen)
          (compileStmt prefix afterThen continuation) elseBranch
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
        (bodyCode, afterBody) = compileStmt prefix (next + 2) (Just conditionLabel) body
    in ([Jump conditionLabel, DefineLabel bodyLabel] ++ bodyCode
        ++ [DefineLabel conditionLabel] ++ compileExpr condition
        ++ [JumpIfNonZero bodyLabel], afterBody)
  DoWhile body condition ->
    let bodyLabel = label "while_cody" next
        conditionLabel = label "while_cond" (next + 1)
        (bodyCode, afterBody) = compileStmt prefix (next + 2) (Just conditionLabel) body
    in ([DefineLabel bodyLabel] ++ bodyCode ++ [DefineLabel conditionLabel]
        ++ compileExpr condition ++ [JumpIfNonZero bodyLabel], afterBody)
  For initial condition step body ->
    compileStmt prefix next continuation (Block [initial, While condition (Block [body, step])])

compileExpr :: Expr -> [Instruction]
compileExpr expression = case expression of
  Var name -> [LoadVar name]
  Const value -> [PushConst value]
  Bin op left right -> compileExpr left ++ compileExpr right ++ [ApplyBinOp op]
  StringLiteral text -> [PushString text]
  ArrayLiteral elements -> concatMap compileExpr elements ++ [MakeArray (length elements)]
  SExpression name elements -> concatMap compileExpr elements ++ [MakeSExpression name (length elements)]
  Index target index -> compileExpr target ++ compileExpr index ++ [LoadIndex]
  Length target -> compileExpr target ++ [ObjectLength]

label :: String -> Int -> String
label kind number = "L_" ++ kind ++ "_" ++ show number

statementNames :: Stmt -> [String]
statementNames statement = case statement of
  ReadVar name -> [name]
  Write expression -> expressionNames expression
  Assign name expression -> name : expressionNames expression
  AssignOp name _ expression -> name : expressionNames expression
  SetIndex target index value -> concatMap expressionNames [target, index, value]
  SetIndexOp target index _ value -> concatMap expressionNames [target, index, value]
  While condition body -> expressionNames condition ++ statementNames body
  DoWhile body condition -> statementNames body ++ expressionNames condition
  For initial condition step body -> expressionNames condition ++ concatMap statementNames [initial, step, body]
  If condition yes no -> expressionNames condition ++ statementNames yes ++ maybe [] statementNames no
  Block statements -> concatMap statementNames statements
  Skip -> []

expressionNames :: Expr -> [String]
expressionNames expression = case expression of
  Var name -> [name]
  Bin _ left right -> expressionNames left ++ expressionNames right
  ArrayLiteral elements -> concatMap expressionNames elements
  SExpression _ elements -> concatMap expressionNames elements
  Index target index -> expressionNames target ++ expressionNames index
  Length target -> expressionNames target
  _ -> []
