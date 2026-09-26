module JsonParser
  ( parseJsonProgram
  ) where

import AST
import Control.Applicative (empty, optional, (<|>))
import Data.Char (chr, isHexDigit, ord)
import Data.Void (Void)
import Numeric (readHex)
import Text.Megaparsec (Parsec, between, eof, errorBundlePretty, many, parse, satisfy, sepBy)
import Text.Megaparsec.Char (char, space1)
import qualified Text.Megaparsec.Char.Lexer as L

data JsonValue
  = JsonObject [(String, JsonValue)]
  | JsonString String
  | JsonNumber Integer

type JsonParser = Parsec Void String

parseJsonProgram :: String -> Either String Program
parseJsonProgram source = do
  value <- case parse (jsonSpace *> jsonValueP <* eof) "<json>" source of
    Left err -> Left (errorBundlePretty err)
    Right result -> Right result
  Program <$> statementsAt "$" value

jsonValueP :: JsonParser JsonValue
jsonValueP = jsonLexeme (jsonObjectP <|> (JsonString <$> jsonStringP) <|> (JsonNumber <$> jsonNumberP))

jsonObjectP :: JsonParser JsonValue
jsonObjectP = JsonObject <$> between (char '{' *> jsonSpace) (char '}')
  (field `sepBy` (char ',' *> jsonSpace))
  where
    field = do
      key <- jsonStringP
      jsonSpace
      _ <- char ':'
      jsonSpace
      value <- jsonValueP
      pure (key, value)

jsonStringP :: JsonParser String
jsonStringP = char '"' *> many character <* char '"'
  where
    character = satisfy (\c -> c /= '"' && c /= '\\' && ord c >= 0x20)
      <|> (char '\\' *> escaped)
    escaped = do
      escape <- satisfy (`elem` ("\"\\/bfnrtu" :: String))
      case escape of
        '"' -> pure '"'; '\\' -> pure '\\'; '/' -> pure '/'
        'b' -> pure '\b'; 'f' -> pure '\f'; 'n' -> pure '\n'
        'r' -> pure '\r'; 't' -> pure '\t'
        _ -> unicodeEscape
    unicodeEscape = do
      code <- hexCode
      if code >= 0xd800 && code <= 0xdbff then do
        _ <- char '\\' *> char 'u'
        low <- hexCode
        if low >= 0xdc00 && low <= 0xdfff
          then pure (chr (0x10000 + (code - 0xd800) * 0x400 + low - 0xdc00))
          else fail "expected a low surrogate after a high surrogate"
      else if code >= 0xdc00 && code <= 0xdfff
        then fail "unexpected low surrogate"
        else pure (chr code)
    hexCode = do
      digits <- sequence (replicate 4 (satisfy isHexDigit))
      case readHex digits of
        [(code, "")] -> pure code
        _ -> fail "invalid Unicode escape"

jsonNumberP :: JsonParser Integer
jsonNumberP = do
  sign <- optional (char '-')
  first <- satisfy (\c -> c >= '0' && c <= '9')
  rest <- if first == '0' then pure [] else many (satisfy (\c -> c >= '0' && c <= '9'))
  let digits = first : rest
  pure (read (case sign of Nothing -> digits; Just _ -> '-' : digits))

jsonSpace :: JsonParser ()
jsonSpace = L.space space1 empty empty

jsonLexeme :: JsonParser a -> JsonParser a
jsonLexeme = L.lexeme jsonSpace

statementsAt :: String -> JsonValue -> Either String [Stmt]
statementsAt path value = case value of
  JsonObject [("seq", payload)] -> do
    (left, right) <- pairAt (path ++ ".seq") "left" "right" payload
    (++) <$> statementsAt (path ++ ".seq.left") left <*> statementsAt (path ++ ".seq.right") right
  _ -> (: []) <$> statementAt path value

statementAt :: String -> JsonValue -> Either String Stmt
statementAt path value = case value of
  JsonString "skip" -> Right Skip
  JsonObject [("read", name)] -> ReadVar <$> stringAt (path ++ ".read") name
  JsonObject [("write", expression)] -> Write <$> expressionAt (path ++ ".write") expression
  JsonObject [("assn", payload)] -> do
    (name, expression) <- pairAt (path ++ ".assn") "dst" "src" payload
    Assign <$> stringAt (path ++ ".assn.dst") name <*> expressionAt (path ++ ".assn.src") expression
  JsonObject [("while", payload)] -> do
    (condition, body) <- pairAt (path ++ ".while") "cond" "body" payload
    While <$> expressionAt (path ++ ".while.cond") condition <*> bodyAt (path ++ ".while.body") body
  JsonObject [("do", payload)] -> do
    (body, condition) <- pairAt (path ++ ".do") "body" "cond" payload
    DoWhile <$> bodyAt (path ++ ".do.body") body <*> expressionAt (path ++ ".do.cond") condition
  JsonObject [("if", payload)] -> do
    (condition, thenBranch, elseBranch) <- tripleAt (path ++ ".if") "cond" "then" "else" payload
    If <$> expressionAt (path ++ ".if.cond") condition
       <*> bodyAt (path ++ ".if.then") thenBranch
       <*> (Just <$> bodyAt (path ++ ".if.else") elseBranch)
  JsonObject [("seq", _)] -> Block <$> statementsAt path value
  _ -> Left (path ++ ": expected a statement (skip, read, write, assn, seq, while, do, or if)")

bodyAt :: String -> JsonValue -> Either String Stmt
bodyAt = statementAt

expressionAt :: String -> JsonValue -> Either String Expr
expressionAt path value = case value of
  JsonObject [("var", name)] -> Var <$> stringAt (path ++ ".var") name
  JsonObject [("const", number)] -> Const <$> intAt (path ++ ".const") number
  JsonObject fields | sameKeys ["binop", "left", "right"] fields -> do
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

stringAt :: String -> JsonValue -> Either String String
stringAt _ (JsonString value) = Right value
stringAt path _ = Left (path ++ ": expected a string")

intAt :: String -> JsonValue -> Either String Int
intAt path (JsonNumber value)
  | value >= toInteger (minBound :: Int) && value <= toInteger (maxBound :: Int) = Right (fromInteger value)
  | otherwise = Left (path ++ ": integer is outside the supported Int range")
intAt path _ = Left (path ++ ": expected an integer")

pairAt :: String -> String -> String -> JsonValue -> Either String (JsonValue, JsonValue)
pairAt path first second (JsonObject fields)
  | sameKeys [first, second] fields = (,) <$> fieldAt path first fields <*> fieldAt path second fields
pairAt path first second _ = Left (path ++ ": expected fields " ++ show [first, second])

tripleAt :: String -> String -> String -> String -> JsonValue -> Either String (JsonValue, JsonValue, JsonValue)
tripleAt path first second third (JsonObject fields)
  | sameKeys [first, second, third] fields =
      (,,) <$> fieldAt path first fields <*> fieldAt path second fields <*> fieldAt path third fields
tripleAt path first second third _ = Left (path ++ ": expected fields " ++ show [first, second, third])

sameKeys :: [String] -> [(String, JsonValue)] -> Bool
sameKeys keys fields = length keys == length fields && all (\key -> length (filter ((== key) . fst) fields) == 1) keys

fieldAt :: String -> String -> [(String, JsonValue)] -> Either String JsonValue
fieldAt path key fields = maybe (Left (path ++ ": missing field " ++ show key)) Right (lookup key fields)

