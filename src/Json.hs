module Json
  ( programToJson
  , programToPrettyJson
  , machineToJson
  , machineToPrettyJson
  ) where

import Data.Char (ord)
import AST
import Data.List (intercalate)
import Machine (Instruction (..), MachineProgram (..))
import Numeric (showHex)

programToJson :: Program -> String
programToJson = renderCompact . programJson

programToPrettyJson :: Program -> String
programToPrettyJson = renderPretty 0 . programJson

machineToJson :: MachineProgram -> String
machineToJson = renderCompact . machineJson

machineToPrettyJson :: MachineProgram -> String
machineToPrettyJson = renderPretty 0 . machineJson

data JsonValue
  = JsonObject [(String, JsonValue)]
  | JsonArray [JsonValue]
  | JsonString String
  | JsonNumber Int

machineJson :: MachineProgram -> JsonValue
machineJson (MachineProgram instructions) = JsonArray (map instructionJson instructions)

instructionJson :: Instruction -> JsonValue
instructionJson instruction = case instruction of
  ReadInput -> string "READ"
  WriteOutput -> string "WRITE"
  LoadVar name -> object [("LD", string name)]
  EnterScope -> string "SCOPE"
  LeaveScope -> string "ENDSCOPE"
  BindVar name -> object [("BIND", string name)]
  StoreVar name -> object [("ST", string name)]
  PushConst value -> object [("CONST", JsonNumber value)]
  PushString text -> object [("STRING", string text)]
  MakeArray count -> object [("ARRAY", JsonNumber count)]
  MakeSExpression name count -> object [("SEXP", JsonArray [string name, JsonNumber count])]
  LoadIndex -> string "ELEM"
  StoreIndex -> string "SET"
  IsType kind -> object [("ISTYPE", string (case kind of
    IntType -> "int"; ArrayType -> "array"; StringType -> "string"; SExpressionType -> "sexp"))]
  IsTag name count -> object [("ISTAG", JsonArray [string name, JsonNumber count])]
  StringEquals text -> object [("STREQ", string text)]
  MatchFail -> string "MATCHFAIL"
  ObjectLength -> string "LENGTH"
  ApplyBinOp op -> object [("BINOP", string (binOpName op))]
  DefineLabel name -> object [("LABEL", string name)]
  Jump name -> object [("JMP", string name)]
  JumpIfZero name -> object [("JZ", string name)]
  JumpIfNonZero name -> object [("JNZ", string name)]

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
  SetIndex target index value -> object [("setindex", object [("target", exprJson target), ("index", exprJson index), ("value", exprJson value)])]
  SetIndexOp target index op value -> object [("setindexop", object [("target", exprJson target), ("index", exprJson index), ("op", string (binOpName op)), ("value", exprJson value)])]
  While condition body -> object [("while", object [("cond", exprJson condition), ("body", stmtJson body)])]
  DoWhile body condition -> object [("do", object [("body", stmtJson body), ("cond", exprJson condition)])]
  For initial condition step body -> sequenceJson [initial, While condition (Block [body, step])]
  If condition thenBranch elseBranch -> object [("if", object [("cond", exprJson condition), ("then", stmtJson thenBranch), ("else", maybe (string "skip") stmtJson elseBranch)])]
  Block statements -> sequenceJson statements
  Switch scrutinee branches -> object [("switch", object
    [("scrutinee", exprJson scrutinee), ("branches", JsonArray (map branchJson branches))])]
    where
      branchJson (patternValue, body) = object
        [("pattern", patternJson patternValue), ("branch", stmtJson body)]
  Skip -> string "skip"

patternJson :: Pattern -> JsonValue
patternJson patternValue = case patternValue of
  Wildcard -> object [("wildcard", JsonArray [])]
  BindPattern name nested -> object [("bind", object
    [("pattern", patternJson nested), ("name", string name)])]
  NumberPattern value -> JsonNumber value
  StringPattern text -> string text
  ArrayPattern patterns -> object [("array", JsonArray (map patternJson patterns))]
  SExpressionPattern name patterns -> object
    [("sexp", string name), ("args", JsonArray (map patternJson patterns))]
  TypePattern kind -> string $ case kind of
    ArrayType -> "#array"
    IntType -> "#int"
    StringType -> "#string"
    SExpressionType -> "#sexp"

exprJson :: Expr -> JsonValue
exprJson expression = case expression of
  Var name -> object [("var", string name)]
  Const value -> object [("const", JsonNumber value)]
  StringLiteral text -> object [("string", string text)]
  ArrayLiteral values -> object [("array", JsonArray (map exprJson values))]
  SExpression name values -> object [("sexp", string name), ("args", JsonArray (map exprJson values))]
  Index target index -> object [("elem", exprJson target), ("index", exprJson index)]
  Length target -> object [("length", exprJson target)]
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
  JsonArray values -> "[" ++ intercalate "," (map renderCompact values) ++ "]"
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
  JsonArray [] -> "[]"
  JsonArray values -> "[\n" ++ intercalate ",\n" (map renderElement values) ++ "\n" ++ indentation level ++ "]"
    where
      renderElement element = indentation (level + 1) ++ renderPretty (level + 1) element
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
