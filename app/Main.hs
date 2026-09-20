module Main (main) where

import AST
import System.Environment (getArgs)
import Interpreter
import Json
import Parser

-- The first input is repetition count. Each following input is a value whose factorial is printed.
factorialProgram :: Program
factorialProgram = Program
  [ ReadVar "rep"
  , While (Var "rep") (Block
      [ AssignOp "rep" Sub (Const 1)
      , ReadVar "n"
      , Assign "f" (Const 1)
      , While (Var "n") (Block
          [ AssignOp "f" Mul (Var "n")
          , AssignOp "n" Sub (Const 1)
          ])
      , Write (Var "f")
      ])
  ]

main :: IO ()
main = do
  arguments <- getArgs
  case arguments of
    [] -> runDemo
    sourceFile : inputValues -> runSourceFile sourceFile inputValues

runDemo :: IO ()
runDemo = do
  putStrLn "AST as JSON:"
  putStrLn (programToJson factorialProgram)
  putStrLn "Result for input [3, 5, 4, 3]:"
  print (runProgram [3, 5, 4, 3] factorialProgram)

runSourceFile :: FilePath -> [String] -> IO ()
runSourceFile sourceFile inputValues = do
  source <- readFile sourceFile
  case (parseProgram source, parseInputs inputValues) of
    (Left parseError, _) -> putStrLn ("Parse error: " ++ parseError)
    (_, Left inputError) -> putStrLn ("Input error: " ++ inputError)
    (Right program, Right values) -> do
      putStrLn "AST as JSON:"
      putStrLn (programToJson program)
      print (runProgram values program)

parseInputs :: [String] -> Either String [Int]
parseInputs = traverse parseInput
  where
    parseInput value = case reads value of
      [(number, "")] -> Right number
      _ -> Left ("expected integer, got " ++ value)
