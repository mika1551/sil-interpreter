module Main (main) where

import AST
import CLI
import Data.Char (isSpace)
import Data.Either (isLeft)
import Data.List (isInfixOf)
import Interpreter
import Json
import Parser
import Test.HUnit

main :: IO Counts
main = runTestTT tests

parseOK :: String -> Program
parseOK source =
  case parseProgram source of
    Right program -> program
    Left err -> error ("parse failed: " ++ err)

mkEq :: (Eq a, Show a) => String -> a -> a -> Test
mkEq label expected actual = TestCase (assertEqual label expected actual)

mkBool :: String -> Bool -> Test
mkBool label condition = TestCase (assertBool label condition)

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

checks :: Test
checks = TestList
  [ mkEq "parse program block" (Program [Assign "x" (Const 5)]) (parseOK "{ x = 5; }")
  , mkEq "parse function-style read/write"
      (Program [ReadVar "n", Write (Var "n")])
      (parseOK "{ read(n); write(n); }")
  , mkEq "parse arithmetic precedence"
      (Program [Assign "x" (Bin Add (Const 1) (Bin Mul (Const 2) (Const 3)))])
      (parseOK "{ x = 1 + 2 * 3; }")
  , mkEq "parse comparison and logic"
      (Program [Assign "flag" (Bin Or (Bin And (Bin Ge (Var "a") (Const 0)) (Bin Le (Var "b") (Const 10))) (Bin Eq (Var "c") (Const 1)))])
      (parseOK "{ flag = ((a >= 0) && (b <= 10)) || (c == 1); }")
  , mkEq "parse while block"
      (Program [While (Bin Gt (Var "n") (Const 0)) (Block [AssignOp "n" Sub (Const 1)])])
      (parseOK "{ while (n > 0) { n -= 1; } }")
  , mkEq "parse if else"
      (Program [If (Bin Eq (Var "x") (Const 0)) (Write (Var "x")) (Just (Write (Const 1)))])
      (parseOK "{ if (x == 0) write(x); else write(1); }")
  , mkEq "parse elif"
      (Program [If (Bin Gt (Var "x") (Const 0)) (Assign "y" (Const 1)) (Just (If (Bin Lt (Var "x") (Const 0)) (Assign "y" (Const 2)) Nothing))])
      (parseOK "{ if (x > 0) y = 1; elif (x < 0) y = 2; }")
  , mkEq "parse do while"
      (Program [DoWhile (Assign "x" (Const 1)) (Bin Gt (Var "x") (Const 0))])
      (parseOK "{ do x = 1; while (x > 0); }")
  , mkEq "parse for loop"
      (Program [For (Assign "i" (Const 0)) (Bin Lt (Var "i") (Const 3)) (AssignOp "i" Add (Const 1)) (Write (Var "i"))])
      (parseOK "{ for (i = 0; i < 3; i += 1) write(i); }")
  , mkEq "parse block and skip"
      (Program [Block [Assign "a" (Const 1), Skip, Write (Var "a")]])
      (parseOK "{ { a = 1; skip; write(a); } }")
  , mkEq "parse comments"
      (Program [Assign "x" (Const 5)])
      (parseOK "{ -- comment\n x = 5; (* block comment *) }")
  ]

runtimeChecks :: Test
runtimeChecks = TestList
  [ mkEq "run simple expression" (Right [3]) (runProgram [] (Program [Assign "x" (Const 3), Write (Var "x")]))
  , mkEq "run read then arithmetic"
      (Right [17])
      (runProgram [10, 7] (Program [ReadVar "x", ReadVar "y", Assign "z" (Bin Add (Var "x") (Var "y")), Write (Var "z")]))
  , mkEq "run all operators"
      (Right [5, 5, 12, 5, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1])
      (runProgram [] (Program
        [ Write (Bin Add (Const 2) (Const 3))
        , Write (Bin Sub (Const 9) (Const 4))
        , Write (Bin Mul (Const 3) (Const 4))
        , Write (Bin Div (Const 10) (Const 2))
        , Write (Bin Mod (Const 10) (Const 3))
        , Write (Bin Eq (Const 5) (Const 5))
        , Write (Bin Neq (Const 1) (Const 2))
        , Write (Bin Lt (Const 2) (Const 5))
        , Write (Bin Le (Const 5) (Const 5))
        , Write (Bin Gt (Const 6) (Const 2))
        , Write (Bin Ge (Const 7) (Const 7))
        , Write (Bin And (Const 1) (Const 1))
        , Write (Bin Or (Const 0) (Const 1))
        , Write (Bin Or (Const 1) (Const 0))
        ]))
  , mkEq "run assignment operators"
      (Right [7])
      (runProgram [] (Program [Assign "x" (Const 3), AssignOp "x" Add (Const 4), Write (Var "x")]))
  , mkEq "run while loop"
      (Right [3, 2, 1])
      (runProgram [] (Program [Assign "x" (Const 3), While (Bin Gt (Var "x") (Const 0)) (Block [Write (Var "x"), AssignOp "x" Sub (Const 1)])]))
  , mkEq "run if true else false"
      (Right [9])
      (runProgram [] (Program [Assign "x" (Const 9), If (Bin Gt (Var "x") (Const 0)) (Write (Var "x")) (Just (Write (Const 0)))]))
  , mkEq "run do while"
      (Right [3, 2, 1])
      (runProgram [] (Program [Assign "x" (Const 3), DoWhile (Block [Write (Var "x"), AssignOp "x" Sub (Const 1)]) (Bin Gt (Var "x") (Const 0))]))
  , mkEq "run for loop"
      (Right [0, 1, 2])
      (runProgram [] (Program [For (Assign "i" (Const 0)) (Bin Lt (Var "i") (Const 3)) (AssignOp "i" Add (Const 1)) (Write (Var "i"))]))
  , mkEq "undefined variable" (Left (UndefinedVariable "x")) (runProgram [] (Program [Write (Var "x")]))
  , mkEq "input exhausted" (Left InputExhausted) (runProgram [] (Program [ReadVar "x"]))
  , mkEq "division by zero" (Left DivisionByZero) (runProgram [] (Program [Assign "x" (Const 1), Write (Bin Div (Var "x") (Const 0))]))
  , mkEq "boolean operator semantics" (Right [0, 1]) (runProgram [] (Program [Write (Bin And (Const 1) (Const 0)), Write (Bin Or (Const 0) (Const 1))]))
  ]

cliChecks :: Test
cliChecks = TestList
  [ mkEq "parse run command" (Right (Run "program.sil" [1, 2, 3])) (parseCommand ["run", "program.sil", "--input", "1", "2", "3"])
  , mkEq "parse comma separated input" (Right (Run "program.sil" [3, 5, 4, 3])) (parseCommand ["run", "program.sil", "-i", "3,5,4,3"])
  , mkEq "parse ast compact" (Right (Ast "program.sil" Compact)) (parseCommand ["ast", "program.sil"])
  , mkEq "parse ast pretty" (Right (Ast "program.sil" Pretty)) (parseCommand ["ast", "program.sil", "--pretty"])
  , mkEq "parse check" (Right (Check "program.sil")) (parseCommand ["check", "program.sil"])
  , mkEq "parse help" (Right Help) (parseCommand ["--help"])
  , mkBool "parse invalid run option" (isLeft (parseCommand ["run", "program.sil", "--bogus"]))
  , mkBool "parse input requires value" (isLeft (parseCommand ["run", "program.sil", "--input"]))
  , mkBool "help text contains commands" (any (\line -> "run" `isInfixOf` trim line && "Execute" `isInfixOf` trim line) (map trim (lines helpText)))
  ]

jsonChecks :: Test
jsonChecks = TestList
  [ mkBool "compact json has program key" ("\"program\"" `isInfixOf` programToJson (Program [Assign "x" (Const 5)]))
  , mkBool "pretty json has nested object" ("\"assign\"" `isInfixOf` programToPrettyJson (Program [Assign "x" (Const 5)]))
  ]

tests :: Test
tests = TestList
  [ TestLabel "parser" checks
  , TestLabel "runtime" runtimeChecks
  , TestLabel "cli" cliChecks
  , TestLabel "json" jsonChecks
  ]
