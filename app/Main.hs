module Main (main) where

import AST (Program)
import CLI
import Control.Exception (IOException, try)
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
  Run sourceFile inputValues
    | takeExtension sourceFile == ".sam" -> withMachine sourceFile $ \program ->
        case Machine.runMachine inputValues program of
          Left runtimeError -> failWith (renderMachineError runtimeError)
          Right valuesWritten -> mapM_ print valuesWritten
    | otherwise -> withProgram sourceFile $ \program ->
        case runProgram inputValues program of
          Left runtimeError -> failWith (renderRuntimeError runtimeError)
          Right valuesWritten -> mapM_ print valuesWritten
  Ast sourceFile _ | takeExtension sourceFile == ".sam" ->
    failWith "ast expects a .sil or .json file"
  Ast sourceFile format -> withProgram sourceFile $ \program ->
    putStrLn $ case format of
      Compact -> programToJson program
      Pretty -> programToPrettyJson program
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
