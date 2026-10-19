module Input
  ( InputValue (..)
  , parseInputValues
  ) where

import Control.Applicative ((<|>))
import Data.Aeson (eitherDecodeStrictText)
import qualified Data.Text as Text
import Data.Void (Void)
import Text.Megaparsec (Parsec, anySingle, between, eof, errorBundlePretty, many, match, notFollowedBy, parse, satisfy, sepBy, sepBy1, try)
import Text.Megaparsec.Char (char, space, space1)
import qualified Text.Megaparsec.Char.Lexer as L

data InputValue
  = InputNumber Int
  | InputString String
  | InputArray [InputValue]
  deriving (Eq, Show)

type Parser = Parsec Void String

parseInputValues :: [String] -> Either String [InputValue]
parseInputValues arguments =
  case parse (space *> valueP `sepBy1` separator <* space <* eof) "--input" (unwords arguments) of
    Left err -> Left (errorBundlePretty err)
    Right values -> Right values
  where
    separator = try (space *> char ',' *> space) <|> try (space1 <* notFollowedBy eof)

valueP :: Parser InputValue
valueP = arrayP <|> stringP <|> numberP

stringP :: Parser InputValue
stringP = do
  (source, _) <- match (between (char '"') (char '"') (many stringChar))
  case eitherDecodeStrictText (Text.pack source) of
    Left err -> fail err
    Right value -> pure (InputString value)
  where
    stringChar = satisfy (\c -> c /= '"' && c /= '\\' && c >= ' ')
      <|> (char '\\' *> anySingle)

arrayP :: Parser InputValue
arrayP = InputArray <$> between (char '[' *> space) (char ']')
  ((valueP <* space) `sepBy` (char ',' *> space))

numberP :: Parser InputValue
numberP = do
  value <- L.signed (pure ()) L.decimal :: Parser Integer
  if value < toInteger (minBound :: Int) || value > toInteger (maxBound :: Int)
    then fail "integer is outside the supported Int range"
    else pure (InputNumber (fromInteger value))
