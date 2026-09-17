module Json (programToJson) where

import AST
import Data.List (intercalate)

-- The coursework grammar restricts identifiers to ASCII letters, digits, '_' and '\'',
-- so no additional string escaping is required here.
programToJson :: Program -> String
programToJson (Program statements) = object [field "program" (array (map stmtToJson statements))]

stmtToJson :: Stmt -> String
stmtToJson statement = case statement of
  ReadVar name -> object [field "read" (string name)]
  Write expression -> object [field "write" (exprToJson expression)]
  Assign name expression -> object [field "assign" (object [field "var" (string name), field "value" (exprToJson expression)])]
  AssignOp name op expression -> object [field "assignOp" (object [field "var" (string name), field "binop" (string (binOpName op)), field "value" (exprToJson expression)])]
  While condition body -> object [field "while" (object [field "cond" (exprToJson condition), field "body" (stmtToJson body)])]
  DoWhile body condition -> object [field "doWhile" (object [field "body" (stmtToJson body), field "cond" (exprToJson condition)])]
  For initial condition step body -> object [field "for" (object [field "init" (stmtToJson initial), field "cond" (exprToJson condition), field "step" (stmtToJson step), field "body" (stmtToJson body)])]
  If condition thenBranch elseBranch -> object [field "if" (object ([field "cond" (exprToJson condition), field "then" (stmtToJson thenBranch)] ++ maybe [] (pure . field "else" . stmtToJson) elseBranch))]
  Block statements -> object [field "block" (array (map stmtToJson statements))]
  Skip -> object [field "skip" "true"]

exprToJson :: Expr -> String
exprToJson expression = case expression of
  Var name -> object [field "var" (string name)]
  Const value -> object [field "const" (show value)]
  Bin op left right -> object [field "binop" (string (binOpName op)), field "left" (exprToJson left), field "right" (exprToJson right)]

binOpName :: BinOp -> String
binOpName op = case op of
  Add -> "+"; Sub -> "-"; Mul -> "*"; Div -> "/"; Mod -> "%"
  Eq -> "=="; Neq -> "!="; Lt -> "<"; Le -> "<="; Gt -> ">"; Ge -> ">="
  And -> "&&"; Or -> "||"

object :: [String] -> String
object fields = "{" ++ intercalate "," fields ++ "}"

field :: String -> String -> String
field key value = string key ++ ":" ++ value

array :: [String] -> String
array values = "[" ++ intercalate "," values ++ "]"

string :: String -> String
string value = "\"" ++ value ++ "\""
