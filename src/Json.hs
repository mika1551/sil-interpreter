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
  | JsonArray [JsonValue]
  | JsonString String
  | JsonNumber Int
  | JsonBool Bool

programJson :: Program -> JsonValue
programJson (Program statements) = object [("program", array (map stmtJson statements))]

stmtJson :: Stmt -> JsonValue
stmtJson statement = case statement of
  ReadVar name -> object [("read", string name)]
  Write expression -> object [("write", exprJson expression)]
  Assign name expression -> object [("assign", object [("var", string name), ("value", exprJson expression)])]
  AssignOp name op expression -> object [("assignOp", object [("var", string name), ("binop", string (binOpName op)), ("value", exprJson expression)])]
  While condition body -> object [("while", object [("cond", exprJson condition), ("body", stmtJson body)])]
  DoWhile body condition -> object [("doWhile", object [("body", stmtJson body), ("cond", exprJson condition)])]
  For initial condition step body -> object [("for", object [("init", stmtJson initial), ("cond", exprJson condition), ("step", stmtJson step), ("body", stmtJson body)])]
  If condition thenBranch elseBranch -> object [("if", object ([("cond", exprJson condition), ("then", stmtJson thenBranch)] ++ maybe [] (pure . (,) "else" . stmtJson) elseBranch))]
  Block statements -> object [("block", array (map stmtJson statements))]
  Skip -> object [("skip", JsonBool True)]

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

array :: [JsonValue] -> JsonValue
array = JsonArray

string :: String -> JsonValue
string = JsonString

renderCompact :: JsonValue -> String
renderCompact value = case value of
  JsonObject fields -> "{" ++ intercalate "," (map renderField fields) ++ "}"
  JsonArray values -> "[" ++ intercalate "," (map renderCompact values) ++ "]"
  JsonString text -> renderString text
  JsonNumber number -> show number
  JsonBool boolean -> if boolean then "true" else "false"
  where
    renderField (key, fieldValue) = renderString key ++ ":" ++ renderCompact fieldValue

renderPretty :: Int -> JsonValue -> String
renderPretty level value = case value of
  JsonObject [] -> "{}"
  JsonObject fields -> "{\n" ++ intercalate ",\n" (map renderField fields) ++ "\n" ++ indentation level ++ "}"
    where
      renderField (key, fieldValue) = indentation (level + 1) ++ renderString key ++ ": " ++ renderPretty (level + 1) fieldValue
  JsonArray [] -> "[]"
  JsonArray values -> "[\n" ++ intercalate ",\n" (map renderElement values) ++ "\n" ++ indentation level ++ "]"
    where
      renderElement element = indentation (level + 1) ++ renderPretty (level + 1) element
  JsonString text -> renderString text
  JsonNumber number -> show number
  JsonBool boolean -> if boolean then "true" else "false"

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
