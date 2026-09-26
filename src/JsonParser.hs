{-# LANGUAGE OverloadedStrings #-}

module JsonParser
  ( parseJsonProgram
  ) where

import AST
import Data.Aeson (Value (..), eitherDecodeStrictText)
import qualified Data.Aeson.Decoding.Text as Decoding
import Data.Aeson.Decoding.Tokens (Number (..), TkArray (..), TkRecord (..), Tokens (..))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Scientific (floatingOrInteger)
import qualified Data.Text as Text

parseJsonProgram :: String -> Either String Program
parseJsonProgram source = do
  let input = Text.pack source
  value <- eitherDecodeStrictText input
  _ <- checkValue (Decoding.textToTokens input)
  Program <$> statementsAt "$" value

checkValue :: Tokens k String -> Either String k
checkValue token = case token of
  TkLit _ next -> Right next
  TkText _ next -> Right next
  TkNumber (NumInteger _) next -> Right next
  TkNumber _ _ -> Left "expected an integer"
  TkArrayOpen items -> checkArray items
  TkRecordOpen fields -> checkRecord [] fields
  TkErr message -> Left message

checkArray :: TkArray k String -> Either String k
checkArray items = case items of
  TkItem value -> checkValue value >>= checkArray
  TkArrayEnd next -> Right next
  TkArrayErr message -> Left message

checkRecord :: [Key.Key] -> TkRecord k String -> Either String k
checkRecord fields record = case record of
  TkPair key value
    | key `elem` fields -> Left ("duplicate JSON field " ++ show (Key.toString key))
    | otherwise -> checkValue value >>= checkRecord (key : fields)
  TkRecordEnd next -> Right next
  TkRecordErr message -> Left message

statementsAt :: String -> Value -> Either String [Stmt]
statementsAt path value = case value of
  Object fields | Just payload <- onlyField "seq" fields -> do
    (left, right) <- pairAt (path ++ ".seq") "left" "right" payload
    (++) <$> statementsAt (path ++ ".seq.left") left <*> statementsAt (path ++ ".seq.right") right
  _ -> (: []) <$> statementAt path value

statementAt :: String -> Value -> Either String Stmt
statementAt path value = case value of
  String "skip" -> Right Skip
  Object fields | Just name <- onlyField "read" fields -> ReadVar <$> stringAt (path ++ ".read") name
  Object fields | Just expression <- onlyField "write" fields -> Write <$> expressionAt (path ++ ".write") expression
  Object fields | Just payload <- onlyField "assn" fields -> do
    (name, expression) <- pairAt (path ++ ".assn") "dst" "src" payload
    Assign <$> stringAt (path ++ ".assn.dst") name <*> expressionAt (path ++ ".assn.src") expression
  Object fields | Just payload <- onlyField "while" fields -> do
    (condition, body) <- pairAt (path ++ ".while") "cond" "body" payload
    While <$> expressionAt (path ++ ".while.cond") condition <*> bodyAt (path ++ ".while.body") body
  Object fields | Just payload <- onlyField "do" fields -> do
    (body, condition) <- pairAt (path ++ ".do") "body" "cond" payload
    DoWhile <$> bodyAt (path ++ ".do.body") body <*> expressionAt (path ++ ".do.cond") condition
  Object fields | Just payload <- onlyField "if" fields -> do
    (condition, thenBranch, elseBranch) <- tripleAt (path ++ ".if") "cond" "then" "else" payload
    If <$> expressionAt (path ++ ".if.cond") condition
       <*> bodyAt (path ++ ".if.then") thenBranch
       <*> (Just <$> bodyAt (path ++ ".if.else") elseBranch)
  Object fields | Just _ <- onlyField "seq" fields -> Block <$> statementsAt path value
  _ -> Left (path ++ ": expected a statement (skip, read, write, assn, seq, while, do, or if)")

bodyAt :: String -> Value -> Either String Stmt
bodyAt = statementAt

expressionAt :: String -> Value -> Either String Expr
expressionAt path value = case value of
  Object fields | Just name <- onlyField "var" fields -> Var <$> stringAt (path ++ ".var") name
  Object fields | Just number <- onlyField "const" fields -> Const <$> intAt (path ++ ".const") number
  Object fields | sameKeys ["binop", "left", "right"] fields -> do
    opValue <- fieldAt path "binop" fields
    leftValue <- fieldAt path "left" fields
    rightValue <- fieldAt path "right" fields
    op <- stringAt (path ++ ".binop") opValue >>= binOpAt (path ++ ".binop")
    Bin op <$> expressionAt (path ++ ".left") leftValue <*> expressionAt (path ++ ".right") rightValue
  _ -> Left (path ++ ": expected an expression (var, const, or binop/left/right)")

binOpAt :: String -> String -> Either String BinOp
binOpAt path name = case lookup name operators of
  Just op -> Right op
  Nothing -> Left (path ++ ": unknown operator " ++ show name)
  where
    operators = [("+", Add), ("-", Sub), ("*", Mul), ("/", Div), ("%", Mod),
      ("==", Eq), ("!=", Neq), ("<", Lt), ("<=", Le), (">", Gt), (">=", Ge),
      ("&&", And), ("!!", Or)]

stringAt :: String -> Value -> Either String String
stringAt _ (String value) = Right (Text.unpack value)
stringAt path _ = Left (path ++ ": expected a string")

intAt :: String -> Value -> Either String Int
intAt path (Number value) = case floatingOrInteger value :: Either Double Integer of
  Right integer
    | integer >= toInteger (minBound :: Int) && integer <= toInteger (maxBound :: Int) -> Right (fromInteger integer)
    | otherwise -> Left (path ++ ": integer is outside the supported Int range")
  Left _ -> Left (path ++ ": expected an integer")
intAt path _ = Left (path ++ ": expected an integer")

pairAt :: String -> String -> String -> Value -> Either String (Value, Value)
pairAt path first second (Object fields)
  | sameKeys [first, second] fields = (,) <$> fieldAt path first fields <*> fieldAt path second fields
pairAt path first second _ = Left (path ++ ": expected fields " ++ show [first, second])

tripleAt :: String -> String -> String -> String -> Value -> Either String (Value, Value, Value)
tripleAt path first second third (Object fields)
  | sameKeys [first, second, third] fields =
      (,,) <$> fieldAt path first fields <*> fieldAt path second fields <*> fieldAt path third fields
tripleAt path first second third _ = Left (path ++ ": expected fields " ++ show [first, second, third])

onlyField :: String -> KeyMap.KeyMap Value -> Maybe Value
onlyField key fields
  | KeyMap.size fields == 1 = KeyMap.lookup (Key.fromString key) fields
  | otherwise = Nothing

sameKeys :: [String] -> KeyMap.KeyMap Value -> Bool
sameKeys keys fields = KeyMap.size fields == length keys && all ((`KeyMap.member` fields) . Key.fromString) keys

fieldAt :: String -> String -> KeyMap.KeyMap Value -> Either String Value
fieldAt path key fields = maybe (Left (path ++ ": missing field " ++ show key)) Right (KeyMap.lookup (Key.fromString key) fields)
