module Main (main) where

import AST
import Interpreter
import Json

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
  putStrLn "AST as JSON:"
  putStrLn (programToJson factorialProgram)
  putStrLn "Result for input [3, 5, 4, 3]:"
  print (runProgram [3, 5, 4, 3] factorialProgram)
