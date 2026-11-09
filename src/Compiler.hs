module Compiler (compileProgram, compileProgramChecked, compileExpr) where

import AST
import Data.List (isPrefixOf)
import qualified PatternMatrix as PM
import Machine

compileProgram :: Program -> MachineProgram
compileProgram (Program statements) = MachineProgram (fst (compileStatements prefix 0 Nothing statements))
  where
    names = concatMap statementNames statements
    prefix = head [replicate n '$' | n <- [1 ..], (not . any (replicate n '$' `isPrefixOf`)) names]

compileProgramChecked :: Program -> Either String MachineProgram
compileProgramChecked = Right . compileProgram

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
  Switch scrutinee branches -> compileSwitch prefix next continuation scrutinee branches
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
  Switch scrutinee branches -> expressionNames scrutinee ++ concatMap (\(p, body) -> map fst (PM.patternBindings p) ++ statementNames body) branches

expressionNames :: Expr -> [String]
expressionNames expression = case expression of
  Var name -> [name]
  Bin _ left right -> expressionNames left ++ expressionNames right
  ArrayLiteral elements -> concatMap expressionNames elements
  SExpression _ elements -> concatMap expressionNames elements
  Index target index -> expressionNames target ++ expressionNames index
  Length target -> expressionNames target
  _ -> []

compileSwitch :: String -> Int -> Maybe String -> Expr -> [(Pattern, Stmt)] -> ([Instruction], Int)
compileSwitch prefix next continuation scrutinee branches =
  let root = prefix ++ "match_" ++ show next
      endLabel = label "match_end" next
      branchLabel i = label "match_branch" next ++ "_" ++ show i
      (decision, afterDecision) = emitDecision root branchLabel (next + 1)
        (PM.compileMatrix (map fst branches))
      emitBodies counter [] = ([], counter)
      emitBodies counter ((i, (p, body)) : rest) =
        let bindings = concat [loadOccurrence root path ++ [BindVar name]
                              | (name, path) <- PM.patternBindings p]
            (code, afterBody) = compileStmt prefix counter (Just endLabel) body
            (remaining, final) = emitBodies afterBody rest
        in ([DefineLabel (branchLabel i), EnterScope] ++ bindings ++ code ++ [Jump endLabel] ++ remaining, final)
      (bodies, finalNext) = emitBodies afterDecision (zip [0 :: Int ..] branches)
      endCode = [DefineLabel endLabel, LeaveScope] ++ maybe [] (\target -> [Jump target]) continuation
  in (compileExpr scrutinee ++ [StoreVar root] ++ decision ++ bodies ++ endCode, finalNext)

loadOccurrence :: String -> PM.Occurrence -> [Instruction]
loadOccurrence root path = LoadVar root : concatMap (\i -> [PushConst i, LoadIndex]) path

emitDecision :: String -> (Int -> String) -> Int -> PM.Decision -> ([Instruction], Int)
emitDecision root branchLabel next decision = case decision of
  PM.Failure -> ([MatchFail], next)
  PM.Success i -> ([Jump (branchLabel i)], next)
  PM.Test path test yes no ->
    let noLabel = label "match_no" next
        (yesCode, afterYes) = emitDecision root branchLabel (next + 1) yes
        (noCode, final) = emitDecision root branchLabel afterYes no
        load = loadOccurrence root path
        predicate = case test of
          PM.TypeHead kind -> load ++ [IsType kind, JumpIfZero noLabel]
          PM.NumberHead n -> load ++ [IsType IntType, JumpIfZero noLabel]
            ++ load ++ [PushConst n, ApplyBinOp Eq, JumpIfZero noLabel]
          PM.StringHead text -> load ++ [StringEquals text, JumpIfZero noLabel]
          PM.ArrayHead count -> load ++ [IsType ArrayType, JumpIfZero noLabel]
            ++ load ++ [ObjectLength, PushConst count, ApplyBinOp Eq, JumpIfZero noLabel]
          PM.ConstructorHead name count -> load ++ [IsTag name count, JumpIfZero noLabel]
    in (predicate ++ yesCode ++ [DefineLabel noLabel] ++ noCode, final)
