module CLI
  ( Command (..)
  , JsonFormat (..)
  , helpText
  , parseCommand
  ) where

import Data.List (intercalate)

data Command
  = Run FilePath [Int]
  | Ast FilePath JsonFormat
  | Check FilePath
  | Help
  | Version
  deriving (Eq, Show)

data JsonFormat = Compact | Pretty
  deriving (Eq, Show)

parseCommand :: [String] -> Either String Command
parseCommand arguments = case arguments of
  [] -> Right Help
  ["help"] -> Right Help
  ["--help"] -> Right Help
  ["-h"] -> Right Help
  ["--version"] -> Right Version
  "run" : rest -> parseRun rest
  "ast" : rest -> parseAst rest
  ["check", sourceFile] -> Right (Check sourceFile)
  "check" : _ -> Left "usage: sil-interpreter check <file>"
  command : _ -> Left ("unknown command '" ++ command ++ "'")

parseRun :: [String] -> Either String Command
parseRun arguments = case arguments of
  [] -> Left "usage: sil-interpreter run <file> [--input <integers...>]"
  sourceFile : options -> Run sourceFile <$> parseRunOptions options

parseRunOptions :: [String] -> Either String [Int]
parseRunOptions options = case options of
  [] -> Right []
  "--input" : values -> parseInputValues values
  "-i" : values -> parseInputValues values
  option : values | "--input=" `prefixOf` option ->
    parseInputValues (drop (length "--input=") option : values)
  option : _ -> Left ("unknown run option '" ++ option ++ "'")

parseInputValues :: [String] -> Either String [Int]
parseInputValues values
  | null values = Left "--input requires at least one integer"
  | otherwise = traverse parseInteger (concatMap splitOnComma values)
  where
    parseInteger inputValue = case reads inputValue of
      [(number, "")] -> Right number
      _ -> Left ("invalid input value '" ++ inputValue ++ "': expected an integer")

parseAst :: [String] -> Either String Command
parseAst arguments = case arguments of
  [sourceFile] -> Right (Ast sourceFile Compact)
  [sourceFile, "--pretty"] -> Right (Ast sourceFile Pretty)
  [] -> Left "usage: sil-interpreter ast <file> [--pretty]"
  _ : option : _ -> Left ("unknown ast option '" ++ option ++ "'")

prefixOf :: String -> String -> Bool
prefixOf prefix value = take (length prefix) value == prefix

splitOnComma :: String -> [String]
splitOnComma value = case break (== ',') value of
  (part, []) -> [part]
  (part, _ : rest) -> part : splitOnComma rest

helpText :: String
helpText = intercalate "\n"
  [ "SIL interpreter"
  , ""
  , "Usage:"
  , "  sil-interpreter run <file> [--input <integers...>]"
  , "  sil-interpreter ast <file> [--pretty]"
  , "  sil-interpreter check <file>"
  , "  sil-interpreter --help"
  , "  sil-interpreter --version"
  , ""
  , "Commands:"
  , "  run    Execute a SIL program and print each value written by it."
  , "  ast    Print the parsed program as JSON."
  , "  check  Validate the syntax of a SIL program."
  , ""
  , "Options:"
  , "  -i, --input <values>  Integers consumed by read; commas are optional."
  , "      --pretty          Pretty-print JSON produced by ast."
  , "  -h, --help            Show this help."
  , "      --version         Show the version."
  ]
