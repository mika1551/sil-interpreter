module Main (main) where

import AST (Program (..), Stmt (..), Expr (..))
import CLI
import Compiler (compileProgram)
import Control.Exception (IOException, try)
import Data.List (intercalate)
import Data.Version (showVersion)
import Interpreter
import Json
import JsonParser (parseJsonProgram)
import Machine (MachineProgram)
import MachineInterpreter (MachineError)
import qualified MachineInterpreter as Machine
import MachineParser (parseMachine)
import Parser
import Paths_sil_interpreter (version)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath (takeExtension)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  arguments <- getArgs
  case parseCommand arguments of
    Left message -> failWith (message ++ "\nTry 'sil-interpreter --help' for usage.")
    Right command -> executeCommand command

executeCommand :: Command -> IO ()
executeCommand command = case command of
  Help -> putStrLn helpText
  Version -> putStrLn ("sil-interpreter " ++ showVersion version)
  RunValues sourceFile inputValues
    | takeExtension sourceFile == ".sam" -> failWith "machine execution does not support string or array inputs"
    | otherwise -> withProgram sourceFile $ \program ->
        case runProgramInput inputValues program of
          Left runtimeError -> failWith (renderRuntimeError runtimeError)
          Right valuesWritten -> mapM_ (putStrLn . renderOutputValue) valuesWritten
  Run sourceFile inputValues
    | takeExtension sourceFile == ".sam" -> withMachine sourceFile $ \program ->
        runMachineAndPrint inputValues program
    | otherwise -> withProgram sourceFile $ \program ->
        case runProgramValues inputValues program of
          Left runtimeError -> failWith (renderRuntimeError runtimeError)
          Right valuesWritten -> mapM_ (putStrLn . renderOutputValue) valuesWritten
  RunMachine sourceFile inputValues
    | takeExtension sourceFile == ".sam" -> withMachine sourceFile $
        runMachineAndPrint inputValues
    | otherwise -> withProgram sourceFile $ \program ->
        if usesExtendedFeatures program
          then failWith "machine execution does not yet support strings and arrays"
          else runMachineAndPrint inputValues (compileProgram program)
  Ast sourceFile _ | takeExtension sourceFile == ".sam" ->
    failWith "ast expects a .sil or .json file"
  Ast sourceFile format -> withProgram sourceFile $ \program ->
    if usesExtendedFeatures program
      then failWith "JSON output does not yet support strings and arrays"
      else putStrLn $ case format of
        Compact -> programToJson program
        Pretty -> programToPrettyJson program
  Compile sourceFile outputFile
    | takeExtension sourceFile == ".sam" ->
        failWith "compile expects a .sil or .json file"
    | otherwise -> withProgram sourceFile $ \program -> do
        if usesExtendedFeatures program
          then failWith "machine compilation does not yet support strings and arrays"
          else do
            let result = machineToPrettyJson (compileProgram program) ++ "\n"
            case outputFile of
              Nothing -> putStr result
              Just path -> do
                writeResult <- try (writeFile path result) :: IO (Either IOException ())
                case writeResult of
                  Left fileError -> failWith ("cannot write '" ++ path ++ "': " ++ show fileError)
                  Right () -> pure ()
  Check sourceFile
    | takeExtension sourceFile == ".sam" -> withMachine sourceFile $ \program ->
        case Machine.validateMachine program of
          Left machineError -> failWith (renderMachineError machineError)
          Right () -> putStrLn (sourceFile ++ ": OK")
    | otherwise -> withProgram sourceFile $ \_ ->
        putStrLn (sourceFile ++ ": OK")

withProgram :: FilePath -> (Program -> IO ()) -> IO ()
withProgram sourceFile action = withSource sourceFile $ \source ->
  case (if takeExtension sourceFile == ".json" then parseJsonProgram else parseProgram) source of
    Left parseError -> failWith (sourceFile ++ ": " ++ parseError)
    Right program -> action program

withMachine :: FilePath -> (MachineProgram -> IO ()) -> IO ()
withMachine sourceFile action = withSource sourceFile $ \source ->
  case parseMachine source of
    Left parseError -> failWith (sourceFile ++ ": " ++ parseError)
    Right program -> action program

runMachineAndPrint :: [Int] -> MachineProgram -> IO ()
runMachineAndPrint inputValues program =
  case Machine.runMachine inputValues program of
    Left runtimeError -> failWith (renderMachineError runtimeError)
    Right valuesWritten -> mapM_ print valuesWritten

withSource :: FilePath -> (String -> IO ()) -> IO ()
withSource sourceFile action = do
  sourceResult <- try (readFile sourceFile) :: IO (Either IOException String)
  case sourceResult of
    Left fileError -> failWith ("cannot read '" ++ sourceFile ++ "': " ++ show fileError)
    Right source -> action source

renderRuntimeError :: StateError -> String
renderRuntimeError runtimeError = case runtimeError of
  UndefinedVariable name -> "runtime error: undefined variable '" ++ name ++ "'"
  InputExhausted -> "runtime error: input exhausted"
  DivisionByZero -> "runtime error: division by zero"
  TypeError message -> "runtime error: " ++ message
  IndexOutOfBounds index size -> "runtime error: index " ++ show index ++ " out of bounds for length " ++ show size
  InvalidCharacter code -> "runtime error: invalid character code " ++ show code
  CyclicOutput -> "runtime error: cannot print a cyclic array"

renderOutputValue :: OutputValue -> String
renderOutputValue value = case value of
  OutputNumber n -> show n
  OutputString text -> text
  OutputArray values -> "[" ++ intercalate "; " (map renderOutputValue values) ++ "]"

usesExtendedFeatures :: Program -> Bool
usesExtendedFeatures (Program statements) = any extendedStmt statements
  where
    extendedStmt statement = case statement of
      ReadVar _ -> False
      Write expression -> extendedExpr expression
      Assign _ expression -> extendedExpr expression
      AssignOp _ _ expression -> extendedExpr expression
      SetIndex _ _ _ -> True
      SetIndexOp _ _ _ _ -> True
      While condition body -> extendedExpr condition || extendedStmt body
      DoWhile body condition -> extendedStmt body || extendedExpr condition
      For initial condition step body -> any extendedStmt [initial, step, body] || extendedExpr condition
      If condition thenBranch elseBranch -> extendedExpr condition || extendedStmt thenBranch || maybe False extendedStmt elseBranch
      Block statements' -> any extendedStmt statements'
      Skip -> False
    extendedExpr expression = case expression of
      Var _ -> False
      Const _ -> False
      Bin _ left right -> extendedExpr left || extendedExpr right
      StringLiteral _ -> True
      ArrayLiteral _ -> True
      Index _ _ -> True
      Length _ -> True

renderMachineError :: MachineError -> String
renderMachineError machineError = case machineError of
  Machine.DuplicateLabel name -> "machine error: duplicate label '" ++ name ++ "'"
  Machine.UnknownLabel name -> "machine error: unknown label '" ++ name ++ "'"
  Machine.StackUnderflow position -> "machine error: stack underflow at instruction " ++ show position
  Machine.UndefinedVariable name -> "machine error: undefined variable '" ++ name ++ "'"
  Machine.InputExhausted -> "machine error: input exhausted"
  Machine.DivisionByZero -> "machine error: division by zero"

failWith :: String -> IO a
failWith message = hPutStrLn stderr ("error: " ++ message) >> exitFailure
