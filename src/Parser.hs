module Parser
  ( ParseError
  , parseProgram
  ) where

import AST
import Control.Applicative ((<|>))
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import Text.ParserCombinators.ReadP
import qualified Text.ParserCombinators.ReadP as R

type ParseError = String

parseProgram :: String -> Either ParseError Program
parseProgram source = case readP_to_S (spacesP *> programP <* spacesP <* eof) source of
  [(program, "")] -> Right program
  [] -> Left "syntax error"
  _ -> Left "ambiguous syntax"

programP :: ReadP Program
programP = do
  symbol "{"
  statements <- many1 stmtP
  symbol "}"
  pure (Program statements)

stmtP :: ReadP Stmt
stmtP =
      blockP
  <|> readP
  <|> writeP
  <|> ifP
  <|> whileP
  <|> doWhileP
  <|> forP
  <|> assignmentP
  <|> skipP

blockP :: ReadP Stmt
blockP = do
  symbol "{"
  statements <- many stmtP
  symbol "}"
  pure (Block statements)

readP :: ReadP Stmt
readP = do
  keyword "read"
  name <- parenthesized identifier
  optionalSemicolon
  pure (ReadVar name)

writeP :: ReadP Stmt
writeP = do
  keyword "write"
  value <- parenthesized exprP
  optionalSemicolon
  pure (Write value)

ifP :: ReadP Stmt
ifP = do
  keyword "if"
  condition <- parenthesized exprP
  thenBranch <- stmtP
  elseBranch <- elsePartP <|> pure Nothing
  pure (If condition thenBranch elseBranch)

elsePartP :: ReadP (Maybe Stmt)
elsePartP =
      (do
        keyword "else"
        Just <$> stmtP)
  <|> (do
        keyword "elif"
        condition <- parenthesized exprP
        thenBranch <- stmtP
        rest <- elsePartP <|> pure Nothing
        pure (Just (If condition thenBranch rest)))

whileP :: ReadP Stmt
whileP = do
  keyword "while"
  condition <- parenthesized exprP
  body <- stmtP
  pure (While condition body)

doWhileP :: ReadP Stmt
doWhileP = do
  keyword "do"
  body <- stmtP
  keyword "while"
  condition <- parenthesized exprP
  optionalSemicolon
  pure (DoWhile body condition)

forP :: ReadP Stmt
forP = do
  keyword "for"
  symbol "("
  initial <- assignmentCore
  symbol ";"
  condition <- exprP
  symbol ";"
  step <- assignmentCore
  symbol ")"
  body <- stmtP
  pure (For initial condition step body)

assignmentP :: ReadP Stmt
assignmentP = do
  statement <- assignmentCore
  optionalSemicolon
  pure statement

assignmentCore :: ReadP Stmt
assignmentCore = do
  name <- identifier
  operation <- assignmentOperator
  expression <- exprP
  pure $ case operation of
    Nothing -> Assign name expression
    Just op -> AssignOp name op expression

assignmentOperator :: ReadP (Maybe BinOp)
assignmentOperator =
      (symbol "=" *> pure Nothing)
  <|> (Just Add <$ symbol "+=")
  <|> (Just Sub <$ symbol "-=")
  <|> (Just Mul <$ symbol "*=")
  <|> (Just Div <$ symbol "/=")
  <|> (Just Mod <$ symbol "%=")

skipP :: ReadP Stmt
skipP = do
  keyword "skip"
  optionalSemicolon
  pure Skip

optionalSemicolon :: ReadP ()
optionalSemicolon = () <$ R.optional (symbol ";")

parenthesized :: ReadP a -> ReadP a
parenthesized parser = between (symbol "(") (symbol ")") parser

exprP :: ReadP Expr
exprP = orP

orP :: ReadP Expr
orP = chainl1 andP (Bin Or <$ symbol "||")

andP :: ReadP Expr
andP = chainl1 comparisonP (Bin And <$ symbol "&&")

comparisonP :: ReadP Expr
comparisonP = chainl1 additiveP (comparisonOperator >>= pure . Bin)

comparisonOperator :: ReadP BinOp
comparisonOperator =
      Eq <$ symbol "=="
  <|> Neq <$ symbol "!="
  <|> Le <$ symbol "<="
  <|> Ge <$ symbol ">="
  <|> Lt <$ symbol "<"
  <|> Gt <$ symbol ">"

additiveP :: ReadP Expr
additiveP = chainl1 multiplicativeP
  ((Bin Add <$ symbol "+") <|> (Bin Sub <$ symbol "-"))

multiplicativeP :: ReadP Expr
multiplicativeP = chainl1 primaryP
  ((Bin Mul <$ symbol "*") <|> (Bin Div <$ symbol "/") <|> (Bin Mod <$ symbol "%"))

primaryP :: ReadP Expr
primaryP =
      Const <$> integer
  <|> Var <$> identifier
  <|> parenthesized exprP

identifier :: ReadP String
identifier = lexeme $ do
  first <- satisfy isAlpha <|> satisfy (== '_')
  rest <- munch (\character -> isAlphaNum character || character == '_')
  pure (first : rest)

integer :: ReadP Int
integer = lexeme $ do
  sign <- option 1 (symbolRaw "-" *> pure (-1))
  digits <- munch1 isDigit
  pure (sign * read digits)

keyword :: String -> ReadP String
keyword name = lexeme $ do
  value <- string name
  next <- look
  if null next || not (isAlphaNum (head next) || head next == '_')
    then pure value
    else pfail

symbol :: String -> ReadP String
symbol value = lexeme (symbolRaw value)

symbolRaw :: String -> ReadP String
symbolRaw = string

lexeme :: ReadP a -> ReadP a
lexeme parser = parser <* spacesP

spacesP :: ReadP ()
spacesP = skipMany (satisfy isSpace <|> lineComment <|> blockComment)
  where
    lineComment = do
      string "--"
      skipMany (satisfy (/= '\n'))
      pure ' '
    blockComment = do
      string "(*"
      manyTill get (string "*)")
      pure ' '
