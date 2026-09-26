module Parser
  ( ParseError
  , parseProgram
  ) where

import AST
import Control.Applicative (empty, optional, (<|>))
import Control.Monad (void)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Void (Void)
import Text.Megaparsec (Parsec, between, eof, errorBundlePretty, many, notFollowedBy, parse, satisfy, some, try)
import Text.Megaparsec.Char (char, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L

type ParseError = String
type Parser = Parsec Void String

parseProgram :: String -> Either ParseError Program
parseProgram source = case parse (spaces *> programP <* eof) "<input>" source of
  Left err -> Left (errorBundlePretty err)
  Right program -> Right program

programP :: Parser Program
programP = Program <$> between (symbol "{") (symbol "}") (some stmtP)

stmtP :: Parser Stmt
stmtP = blockP <|> readP <|> writeP <|> ifP <|> whileP
    <|> doWhileP <|> forP <|> skipP <|> assignmentP

blockP :: Parser Stmt
blockP = Block <$> between (symbol "{") (symbol "}") (many stmtP)

readP :: Parser Stmt
readP = do
  keyword "read"
  name <- parenthesized identifier
  optionalSemicolon
  pure (ReadVar name)

writeP :: Parser Stmt
writeP = do
  keyword "write"
  value <- parenthesized exprP
  optionalSemicolon
  pure (Write value)

ifP :: Parser Stmt
ifP = do
  keyword "if"
  condition <- parenthesized exprP
  thenBranch <- stmtP
  elseBranch <- optional elsePartP
  pure (If condition thenBranch elseBranch)

elsePartP :: Parser Stmt
elsePartP = (keyword "else" *> stmtP) <|> do
  keyword "elif"
  condition <- parenthesized exprP
  thenBranch <- stmtP
  rest <- optional elsePartP
  pure (If condition thenBranch rest)

whileP :: Parser Stmt
whileP = do
  keyword "while"
  condition <- parenthesized exprP
  While condition <$> stmtP

doWhileP :: Parser Stmt
doWhileP = do
  keyword "do"
  body <- stmtP
  keyword "while"
  condition <- parenthesized exprP
  optionalSemicolon
  pure (DoWhile body condition)

forP :: Parser Stmt
forP = do
  keyword "for"
  symbol "("
  initial <- assignmentCore
  symbol ";"
  condition <- exprP
  symbol ";"
  step <- assignmentCore
  symbol ")"
  For initial condition step <$> stmtP

assignmentP :: Parser Stmt
assignmentP = assignmentCore <* optionalSemicolon

assignmentCore :: Parser Stmt
assignmentCore = do
  name <- identifier
  operation <- assignmentOperator
  expression <- exprP
  pure $ case operation of
    Nothing -> Assign name expression
    Just op -> AssignOp name op expression

assignmentOperator :: Parser (Maybe BinOp)
assignmentOperator = (Nothing <$ symbol "=")
  <|> (Just Add <$ symbol "+=") <|> (Just Sub <$ symbol "-=")
  <|> (Just Mul <$ symbol "*=") <|> (Just Div <$ symbol "/=")
  <|> (Just Mod <$ symbol "%=")

skipP :: Parser Stmt
skipP = Skip <$ (keyword "skip" *> optionalSemicolon)

optionalSemicolon :: Parser ()
optionalSemicolon = void (optional (symbol ";"))

parenthesized :: Parser a -> Parser a
parenthesized = between (symbol "(") (symbol ")")

exprP :: Parser Expr
exprP = orP

orP :: Parser Expr
orP = chainLeft andP (Bin Or <$ symbol "!!")

andP :: Parser Expr
andP = chainLeft comparisonP (Bin And <$ symbol "&&")

comparisonP :: Parser Expr
comparisonP = chainLeft additiveP (Bin <$> comparisonOperator)

comparisonOperator :: Parser BinOp
comparisonOperator = Eq <$ symbol "==" <|> Neq <$ symbol "!="
  <|> Le <$ symbol "<=" <|> Ge <$ symbol ">="
  <|> Lt <$ symbol "<" <|> Gt <$ symbol ">"

additiveP :: Parser Expr
additiveP = chainLeft multiplicativeP
  (Bin Add <$ symbol "+" <|> Bin Sub <$ symbol "-")

multiplicativeP :: Parser Expr
multiplicativeP = chainLeft primaryP
  (Bin Mul <$ symbol "*" <|> Bin Div <$ symbol "/" <|> Bin Mod <$ symbol "%")

chainLeft :: Parser a -> Parser (a -> a -> a) -> Parser a
chainLeft operand operator = do
  first <- operand
  rest <- many ((,) <$> operator <*> operand)
  pure (foldl (\left (combine, right) -> combine left right) first rest)

primaryP :: Parser Expr
primaryP = Const <$> integer <|> Var <$> identifier <|> parenthesized exprP

identifier :: Parser String
identifier = lexeme $ try $ do
  first <- satisfy isAsciiLower
  rest <- many (satisfy identifierContinue)
  let name = first : rest
  if name `elem` reservedWords then empty else pure name

reservedWords :: [String]
reservedWords = ["read", "write", "if", "else", "elif", "while", "do", "for", "skip"]

identifierContinue :: Char -> Bool
identifierContinue c = isAsciiLower c || isAsciiUpper c || isDigit c || c == '_' || c == '\''

integer :: Parser Int
integer = lexeme $ do
  sign <- optional (char '-')
  digits <- some (satisfy isDigit)
  pure $ case sign of
    Nothing -> read digits
    Just _ -> negate (read digits)

keyword :: String -> Parser ()
keyword name = void (lexeme (try (string name *> notFollowedBy (satisfy identifierContinue))))

symbol :: String -> Parser ()
symbol value = void (lexeme (string value))

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaces

spaces :: Parser ()
spaces = L.space space1 (L.skipLineComment "--") (L.skipBlockComment "(*" "*)")
