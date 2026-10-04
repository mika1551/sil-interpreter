module Main (main) where

import AST
import CLI
import Compiler (compileProgram)
import Data.Char (isSpace)
import Data.Either (isLeft)
import Data.List (isInfixOf)
import Interpreter
import Json
import JsonParser (parseJsonProgram)
import qualified Machine as M
import qualified MachineInterpreter as MI
import MachineParser (parseMachine)
import Parser
import System.Exit (exitFailure)
import Test.HUnit

main :: IO ()
main = do
  result <- runTestTT tests
  if errors result + failures result == 0 then pure () else exitFailure

parseOK :: String -> Program
parseOK source =
  case parseProgram source of
    Right program -> program
    Left err -> error ("parse failed: " ++ err)

parseJsonOK :: String -> Program
parseJsonOK source = case parseJsonProgram source of
  Right program -> program
  Left err -> error ("JSON parse failed: " ++ err)

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
      (parseOK "{ flag = ((a >= 0) && (b <= 10)) !! (c == 1); }")
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
  , mkEq "keyword prefix and apostrophe in identifier"
      (Program [Assign "read'value" (Const 2), Write (Var "read'value")])
      (parseOK "{ read'value = 2; write(read'value); }")
  , mkEq "or has lower precedence than and"
      (Program [Write (Bin Or (Const 1) (Bin And (Const 0) (Const 1)))])
      (parseOK "{ write(1 !! 0 && 1); }")
  , mkBool "reject legacy or spelling" (isLeft (parseProgram "{ write(1 || 0); }"))
  , mkBool "reject reserved word as identifier" (isLeft (parseProgram "{ if = 1; }"))
  , mkBool "reject unclosed comment" (isLeft (parseProgram "{ skip; (* unfinished }"))
  , mkBool "parse error includes source position"
      (case parseProgram "{\nwrite(1 + );\n}" of
        Left message -> "2:" `isInfixOf` message
        Right _ -> False)
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
  , mkEq "parse machine mode before input" (Right (RunMachine "program.sil" [1, 2, 3])) (parseCommand ["run", "program.sil", "--machine", "--input", "1", "2", "3"])
  , mkEq "parse machine mode after input" (Right (RunMachine "program.json" [1, 2, 3])) (parseCommand ["run", "program.json", "--input", "1", "2", "3", "--machine"])
  , mkEq "parse machine mode without input" (Right (RunMachine "program.sil" [])) (parseCommand ["run", "program.sil", "--machine"])
  , mkEq "parse machine run command" (Right (Run "program.sam" [1, 2, 3])) (parseCommand ["run", "program.sam", "--input", "1", "2", "3"])
  , mkEq "parse comma separated input" (Right (Run "program.sil" [3, 5, 4, 3])) (parseCommand ["run", "program.sil", "-i", "3,5,4,3"])
  , mkEq "parse ast compact" (Right (Ast "program.sil" Compact)) (parseCommand ["ast", "program.sil"])
  , mkEq "parse ast pretty" (Right (Ast "program.sil" Pretty)) (parseCommand ["ast", "program.sil", "--pretty"])
  , mkEq "parse check" (Right (Check "program.sil")) (parseCommand ["check", "program.sil"])
  , mkEq "parse machine check" (Right (Check "program.sam")) (parseCommand ["check", "program.sam"])
  , mkEq "parse compile to stdout" (Right (Compile "program.sil" Nothing)) (parseCommand ["compile", "program.sil"])
  , mkEq "parse compile to file" (Right (Compile "program.sil" (Just "program.sam"))) (parseCommand ["compile", "program.sil", "-o", "program.sam"])
  , mkEq "parse help" (Right Help) (parseCommand ["--help"])
  , mkBool "parse invalid run option" (isLeft (parseCommand ["run", "program.sil", "--bogus"]))
  , mkBool "reject repeated machine flag" (isLeft (parseCommand ["run", "program.sil", "--machine", "--machine"]))
  , mkBool "parse input requires value" (isLeft (parseCommand ["run", "program.sil", "--input"]))
  , mkBool "help text contains commands" (any (\line -> "run" `isInfixOf` trim line && "Execute" `isInfixOf` trim line) (map trim (lines helpText)))
  , mkBool "help text mentions machine programs" (".sam" `isInfixOf` helpText)
  ]

jsonChecks :: Test
jsonChecks = TestList
  [ mkEq "skip is a string" "\"skip\"" (programToJson (Program [Skip]))
  , mkEq "assignment shape" "{\"assn\":{\"dst\":\"x\",\"src\":{\"const\":5}}}" (programToJson (Program [Assign "x" (Const 5)]))
  , mkEq "right nested sequence and block" "{\"seq\":{\"left\":{\"read\":\"x\"},\"right\":{\"seq\":{\"left\":{\"write\":{\"var\":\"x\"}},\"right\":\"skip\"}}}}" (programToJson (Program [ReadVar "x", Block [Write (Var "x"), Skip]]))
  , mkEq "compound assignment is expanded" "{\"assn\":{\"dst\":\"x\",\"src\":{\"binop\":\"+\",\"left\":{\"var\":\"x\"},\"right\":{\"const\":2}}}}" (programToJson (Program [AssignOp "x" Add (Const 2)]))
  , mkEq "if always has else" "{\"if\":{\"cond\":{\"const\":1},\"then\":\"skip\",\"else\":\"skip\"}}" (programToJson (Program [If (Const 1) Skip Nothing]))
  , mkBool "pretty json uses target fields" ("\"assn\"" `isInfixOf` programToPrettyJson (Program [Assign "x" (Const 5)]))
  , mkEq "parse JSON sequence and execute it" (Right [5])
      (runProgram [] (parseJsonOK "{\"seq\":{\"left\":{\"assn\":{\"dst\":\"x\",\"src\":{\"const\":5}}},\"right\":{\"write\":{\"var\":\"x\"}}}}"))
  , mkEq "parse JSON control flow and nested sequence" (Right [2, 1])
      (runProgram [] (parseJsonOK (programToJson (parseOK "{ x = 2; while (x > 0) { write(x); x -= 1; } }"))))
  , mkEq "parse JSON do and if" (Right [1, 2])
      (runProgram [] (parseJsonOK (programToJson (parseOK "{ x = 0; do { x += 1; write(x); } while (x < 2); if (x == 2) skip; else write(9); }"))))
  , mkEq "parse JSON all binary operators" (programToJson (parseOK "{ write(1 + 2 - 3 * 4 / 5 % 6 == 7 != 8 < 9 <= 10 > 11 >= 12 && 13 !! 14); }"))
      (programToJson (parseJsonOK (programToJson (parseOK "{ write(1 + 2 - 3 * 4 / 5 % 6 == 7 != 8 < 9 <= 10 > 11 >= 12 && 13 !! 14); }"))))
  , mkEq "JSON string escapes" (Program [ReadVar "x\n\x1f600"])
      (parseJsonOK "{\"read\":\"x\\n\\uD83D\\uDE00\"}")
  , mkBool "reject malformed JSON" (isLeft (parseJsonProgram "{\"write\":{\"const\":1,}}"))
  , mkBool "reject unknown statement" (isLeft (parseJsonProgram "{\"unknown\":1}"))
  , mkBool "reject missing expression field" (isLeft (parseJsonProgram "{\"write\":{\"binop\":\"+\",\"left\":{\"const\":1}}"))
  , mkBool "reject duplicate fields" (isLeft (parseJsonProgram "{\"assn\":{\"dst\":\"x\",\"dst\":\"y\",\"src\":{\"const\":1}}}"))
  , mkBool "reject escaped duplicate fields" (isLeft (parseJsonProgram "{\"assn\":{\"dst\":\"x\",\"d\\u0073t\":\"y\",\"src\":{\"const\":1}}}"))
  , mkBool "reject unknown operator" (isLeft (parseJsonProgram "{\"write\":{\"binop\":\"^\",\"left\":{\"const\":1},\"right\":{\"const\":2}}}"))
  , mkBool "reject integer overflow" (isLeft (parseJsonProgram "{\"write\":{\"const\":999999999999999999999999999999999999}}"))
  , mkBool "reject fractional constants" (isLeft (parseJsonProgram "{\"write\":{\"const\":1.5}}"))
  , mkBool "reject decimal integer constants" (isLeft (parseJsonProgram "{\"write\":{\"const\":1.0}}"))
  , mkBool "reject exponent constants" (isLeft (parseJsonProgram "{\"write\":{\"const\":1e0}}"))
  , mkBool "reject invalid Unicode surrogate" (isLeft (parseJsonProgram "{\"read\":\"\\uD800\"}"))
  ]

machineChecks :: Test
machineChecks = TestList
  [ mkEq "empty machine program" (Right []) (MI.runMachine [] (M.MachineProgram []))
  , mkEq "machine read, store, load, and write" (Right [12])
      (MI.runMachine [12] (M.MachineProgram
        [M.ReadInput, M.StoreVar "x", M.LoadVar "x", M.WriteOutput]))
  , mkEq "machine preserves binary operand order" (Right [6])
      (MI.runMachine [] (M.MachineProgram
        [M.PushConst 8, M.PushConst 2, M.ApplyBinOp Sub, M.WriteOutput]))
  , mkEq "zero branch consumes condition" (Right [7])
      (MI.runMachine [] (M.MachineProgram
        [M.PushConst 7, M.PushConst 0, M.JumpIfZero "done", M.DefineLabel "done", M.WriteOutput]))
  , mkEq "untaken zero branch and forward jump" (Right [10])
      (MI.runMachine [] (M.MachineProgram
        [ M.PushConst 1, M.JumpIfZero "otherwise", M.PushConst 10
        , M.Jump "done", M.DefineLabel "otherwise", M.PushConst 20
        , M.DefineLabel "done", M.WriteOutput
        ]))
  , mkEq "nonzero branch runs loop" (Right [3, 2, 1])
      (MI.runMachine [] (M.MachineProgram
        [ M.PushConst 3, M.StoreVar "x", M.DefineLabel "loop"
        , M.LoadVar "x", M.WriteOutput, M.LoadVar "x", M.PushConst 1
        , M.ApplyBinOp Sub, M.StoreVar "x", M.LoadVar "x"
        , M.JumpIfNonZero "loop"
        ]))
  , mkEq "machine stack underflow" (Left (MI.StackUnderflow 0))
      (MI.runMachine [] (M.MachineProgram [M.WriteOutput]))
  , mkEq "machine undefined variable" (Left (MI.UndefinedVariable "x"))
      (MI.runMachine [] (M.MachineProgram [M.LoadVar "x"]))
  , mkEq "machine input exhausted" (Left MI.InputExhausted)
      (MI.runMachine [] (M.MachineProgram [M.ReadInput]))
  , mkEq "machine division by zero" (Left MI.DivisionByZero)
      (MI.runMachine [] (M.MachineProgram [M.PushConst 1, M.PushConst 0, M.ApplyBinOp Div]))
  , mkEq "machine duplicate label" (Left (MI.DuplicateLabel "here"))
      (MI.runMachine [] (M.MachineProgram [M.DefineLabel "here", M.DefineLabel "here"]))
  , mkEq "machine unknown label" (Left (MI.UnknownLabel "missing"))
      (MI.runMachine [] (M.MachineProgram [M.Jump "missing"]))
  , mkEq "validate machine labels" (Left (MI.UnknownLabel "missing"))
      (MI.validateMachine (M.MachineProgram [M.Jump "missing"]))
  , TestList (map matchingOperator operators)
  ]
  where
    operators = [Add, Sub, Mul, Div, Mod, Eq, Neq, Lt, Le, Gt, Ge, And, Or]
    matchingOperator op = mkEq ("machine matches direct operator " ++ show op)
      (runProgram [] (Program [Write (Bin op (Const 8) (Const 2))]))
      (case MI.runMachine [] (M.MachineProgram
        [M.PushConst 8, M.PushConst 2, M.ApplyBinOp op, M.WriteOutput]) of
        Right result -> Right result
        Left err -> error (show err))

machineParserChecks :: Test
machineParserChecks = TestList
  [ mkEq "parse empty machine program" (Right (M.MachineProgram [])) (parseMachine "[]")
  , mkEq "parse every machine instruction"
      (Right (M.MachineProgram
        [ M.ReadInput, M.WriteOutput, M.LoadVar "x", M.StoreVar "x"
        , M.PushConst (-2), M.ApplyBinOp Add, M.DefineLabel "end"
        , M.Jump "end", M.JumpIfZero "end", M.JumpIfNonZero "end"
        ]))
      (parseMachine "[\"READ\",\"WRITE\",{\"LD\":\"x\"},{\"ST\":\"x\"},{\"CONST\":-2},{\"BINOP\":\"+\"},{\"LABEL\":\"end\"},{\"JMP\":\"end\"},{\"JZ\":\"end\"},{\"JNZ\":\"end\"}]")
  , mkEq "parsed machine program executes" (Right [9])
      (case parseMachine "[\"READ\",{\"ST\":\"x\"},{\"LD\":\"x\"},\"WRITE\"]" of
        Left err -> error err
        Right program -> MI.runMachine [9] program)
  , mkBool "reject non-array machine root" (isLeft (parseMachine "{\"CONST\":1}"))
  , mkBool "reject unknown machine instruction" (isLeft (parseMachine "[\"PUSH\"]"))
  , mkBool "reject missing machine operand" (isLeft (parseMachine "[\"LD\"]"))
  , mkBool "reject extra instruction fields" (isLeft (parseMachine "[{\"LD\":\"x\",\"ST\":\"y\"}]"))
  , mkBool "reject duplicate instruction fields" (isLeft (parseMachine "[{\"LD\":\"x\",\"LD\":\"y\"}]"))
  , mkBool "reject unknown machine operator" (isLeft (parseMachine "[{\"BINOP\":\"^\"}]"))
  , mkBool "reject incorrect machine operand type" (isLeft (parseMachine "[{\"JMP\":3}]"))
  , mkBool "reject fractional machine constant" (isLeft (parseMachine "[{\"CONST\":1.5}]"))
  , mkBool "reject decimal machine constant" (isLeft (parseMachine "[{\"CONST\":1.0}]"))
  , mkBool "reject overflowing machine constant" (isLeft (parseMachine "[{\"CONST\":999999999999999999999999999999999999}]"))
  , mkBool "machine parse error identifies instruction"
      (case parseMachine "[{\"CONST\":1},{\"ST\":2}]" of
        Left err -> "$[1].ST" `isInfixOf` err
        Right _ -> False)
  ]

compilerChecks :: Test
compilerChecks = TestList
  [ mkEq "skip compiles to an empty machine" (M.MachineProgram []) (compileProgram (Program [Skip]))
  , mkEq "straight-line compilation follows stack operand order"
      (M.MachineProgram [M.ReadInput, M.StoreVar "x", M.LoadVar "x", M.PushConst 2,
                         M.ApplyBinOp Sub, M.StoreVar "x", M.LoadVar "x", M.WriteOutput])
      (compileProgram (parseOK "{ read(x); x -= 2; write(x); }"))
  , mkEq "compiled instructions round-trip through SAM JSON"
      (Right (compileProgram (parseOK "{ if (1) write(2); else write(3); }")))
      (parseMachine (machineToJson (compileProgram (parseOK "{ if (1) write(2); else write(3); }"))))
  , mkEq "compiled expression keeps eager boolean evaluation"
      (Left MI.DivisionByZero)
      (MI.runMachine [] (compileProgram (parseOK "{ write(0 && (1 / 0)); }")))
  , TestList (map matches directCases)
  , TestList (map matchesWithInput inputCases)
  ]
  where
    directCases =
      [ "{ if (0) write(1); else write(2); }"
      , "{ if (1) write(1); else write(2); }"
      , "{ if (0) write(1); }"
      , "{ x = 0; while (x < 3) { write(x); x += 1; } }"
      , "{ x = 0; do { write(x); x += 1; } while (x < 3); }"
      , "{ for (i = 0; i < 3; i += 1) write(i); }"
      , "{ x = 2; while (x > 0) { y = 2; while (y > 0) { write(x * y); y -= 1; } x -= 1; } }"
      ]
    inputCases =
      [ ([4, 5], "{ read(x); read(y); if (x < y) write(y); else write(x); }")
      , ([0], "{ read(x); if (x) write(1); else write(2); }")
      ]
    matches source = matchesWithInput ([], source)
    matchesWithInput (values, source) =
      let ast = parseOK source
          machine = compileProgram ast
      in TestList
        [ mkEq ("valid labels: " ++ source) (Right ()) (MI.validateMachine machine)
        , mkEq ("same output: " ++ source) (runProgram values ast)
            (case MI.runMachine values machine of
              Right result -> Right result
              Left err -> error (show err))
        ]

tests :: Test
tests = TestList
  [ TestLabel "parser" checks
  , TestLabel "runtime" runtimeChecks
  , TestLabel "cli" cliChecks
  , TestLabel "json" jsonChecks
  , TestLabel "machine" machineChecks
  , TestLabel "machine parser" machineParserChecks
  , TestLabel "compiler" compilerChecks
  ]
