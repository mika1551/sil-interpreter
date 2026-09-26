module Json
  ( programToJson
  , programToPrettyJson
  ) where

import Data.Char (ord)
import AST
import Data.List (intercalate)
import Numeric (showHex)

programToJson :: Program -> String
programToJson = renderCompact . programJson

programToPrettyJson :: Program -> String
programToPrettyJson = renderPretty 0 . programJson

data JsonValue
  = JsonObject [(String, JsonValue)]
  | JsonString String
  | JsonNumber Int

programJson :: Program -> JsonValue
programJson (Program statements) = sequenceJson statements

sequenceJson :: [Stmt] -> JsonValue
sequenceJson statements = case statements of
  [] -> string "skip"
  [statement] -> stmtJson statement
  statement : rest -> object [("seq", object [("left", stmtJson statement), ("right", sequenceJson rest)])]

stmtJson :: Stmt -> JsonValue
stmtJson statement = case statement of
  ReadVar name -> object [("read", string name)]
  Write expression -> object [("write", exprJson expression)]
  Assign name expression -> object [("assn", object [("dst", string name), ("src", exprJson expression)])]
  AssignOp name op expression -> stmtJson (Assign name (Bin op (Var name) expression))
  While condition body -> object [("while", object [("cond", exprJson condition), ("body", stmtJson body)])]
  DoWhile body condition -> object [("do", object [("body", stmtJson body), ("cond", exprJson condition)])]
  For initial condition step body -> sequenceJson [initial, While condition (Block [body, step])]
  If condition thenBranch elseBranch -> object [("if", object [("cond", exprJson condition), ("then", stmtJson thenBranch), ("else", maybe (string "skip") stmtJson elseBranch)])]
  Block statements -> sequenceJson statements
  Skip -> string "skip"

exprJson :: Expr -> JsonValue
exprJson expression = case expression of
  Var name -> object [("var", string name)]
  Const value -> object [("const", JsonNumber value)]
  Bin op left right -> object [("binop", string (binOpName op)), ("left", exprJson left), ("right", exprJson right)]

binOpName :: BinOp -> String
binOpName op = case op of
  Add -> "+"; Sub -> "-"; Mul -> "*"; Div -> "/"; Mod -> "%"
  Eq -> "=="; Neq -> "!="; Lt -> "<"; Le -> "<="; Gt -> ">"; Ge -> ">="
  And -> "&&"; Or -> "!!"

object :: [(String, JsonValue)] -> JsonValue
object = JsonObject

string :: String -> JsonValue
string = JsonString

renderCompact :: JsonValue -> String
renderCompact value = case value of
  JsonObject fields -> "{" ++ intercalate "," (map renderField fields) ++ "}"
  JsonString text -> renderString text
  JsonNumber number -> show number
  where
    renderField (key, fieldValue) = renderString key ++ ":" ++ renderCompact fieldValue

renderPretty :: Int -> JsonValue -> String
renderPretty level value = case value of
  JsonObject [] -> "{}"
  JsonObject fields -> "{\n" ++ intercalate ",\n" (map renderField fields) ++ "\n" ++ indentation level ++ "}"
    where
      renderField (key, fieldValue) = indentation (level + 1) ++ renderString key ++ ": " ++ renderPretty (level + 1) fieldValue
  JsonString text -> renderString text
  JsonNumber number -> show number

indentation :: Int -> String
indentation level = replicate (level * 2) ' '

renderString :: String -> String
renderString value = "\"" ++ concatMap escape value ++ "\""
  where
    escape character = case character of
      '\"' -> "\\\""
      '\\' -> "\\\\"
      '\b' -> "\\b"
      '\f' -> "\\f"
      '\n' -> "\\n"
      '\r' -> "\\r"
      '\t' -> "\\t"
      _ | ord character < 0x20 -> "\\u" ++ padHex (showHex (ord character) "")
        | otherwise -> [character]
    padHex digits = replicate (4 - length digits) '0' ++ digits
