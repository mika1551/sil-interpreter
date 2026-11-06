module Main (main) where

import AST (Program)
import CLI
import Compiler (compileProgramChecked)
import Control.Exception (IOException, try)
import Data.Version (showVersion)
import Input (InputValue (..))
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
    | takeExtension sourceFile == ".sam" -> withMachine sourceFile $ runMachineInputAndPrint inputValues
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
    | otherwise -> withCompiledProgram sourceFile $ runMachineAndPrint inputValues
  RunMachineValues sourceFile inputValues
    | takeExtension sourceFile == ".sam" -> withMachine sourceFile $ runMachineInputAndPrint inputValues
    | otherwise -> withCompiledProgram sourceFile $ runMachineInputAndPrint inputValues
  Ast sourceFile _ | takeExtension sourceFile == ".sam" ->
    failWith "ast expects a .sil or .json file"
  Ast sourceFile format -> withProgram sourceFile $ \program ->
    putStrLn $ case format of
        Compact -> programToJson program
        Pretty -> programToPrettyJson program
  Compile sourceFile outputFile
    | takeExtension sourceFile == ".sam" ->
        failWith "compile expects a .sil or .json file"
    | otherwise -> withCompiledProgram sourceFile $ \program -> do
        let result = machineToPrettyJson program ++ "\n"
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

withCompiledProgram :: FilePath -> (MachineProgram -> IO ()) -> IO ()
withCompiledProgram sourceFile action = withProgram sourceFile $ \program ->
  case compileProgramChecked program of
    Left message -> failWith message
    Right compiled -> action compiled

withMachine :: FilePath -> (MachineProgram -> IO ()) -> IO ()
withMachine sourceFile action = withSource sourceFile $ \source ->
  case parseMachine source of
    Left parseError -> failWith (sourceFile ++ ": " ++ parseError)
    Right program -> action program

runMachineAndPrint :: [Int] -> MachineProgram -> IO ()
runMachineAndPrint inputValues = runMachineInputAndPrint (map InputNumber inputValues)

runMachineInputAndPrint :: [InputValue] -> MachineProgram -> IO ()
runMachineInputAndPrint inputValues program =
  case Machine.runMachineInput inputValues program of
    Left runtimeError -> failWith (renderMachineError runtimeError)
    Right valuesWritten -> mapM_ (putStrLn . renderOutputValue) valuesWritten

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
  CyclicOutput -> "runtime error: cannot print a cyclic object"
  NoMatchingPattern -> "runtime error: no matching switch case"

renderMachineError :: MachineError -> String
renderMachineError machineError = case machineError of
  Machine.DuplicateLabel name -> "machine error: duplicate label '" ++ name ++ "'"
  Machine.UnknownLabel name -> "machine error: unknown label '" ++ name ++ "'"
  Machine.StackUnderflow position -> "machine error: stack underflow at instruction " ++ show position
  Machine.UndefinedVariable name -> "machine error: undefined variable '" ++ name ++ "'"
  Machine.InputExhausted -> "machine error: input exhausted"
  Machine.DivisionByZero -> "machine error: division by zero"
  Machine.TypeError message -> "machine error: " ++ message
  Machine.IndexOutOfBounds index size -> "machine error: index " ++ show index ++ " out of bounds for length " ++ show size
  Machine.InvalidCharacter code -> "machine error: invalid character code " ++ show code
  Machine.CyclicOutput -> "machine error: cannot print a cyclic object"

failWith :: String -> IO a
failWith message = hPutStrLn stderr ("error: " ++ message) >> exitFailure
