module Main (main) where

import AST (Program)
import CLI
import Control.Exception (IOException, try)
import Data.Version (showVersion)
import Interpreter
import Json
import Parser
import Paths_sil_interpreter (version)
import System.Environment (getArgs)
import System.Exit (exitFailure)
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
  Run sourceFile inputValues -> withProgram sourceFile $ \program ->
    case runProgram inputValues program of
      Left runtimeError -> failWith (renderRuntimeError runtimeError)
      Right valuesWritten -> mapM_ print valuesWritten
  Ast sourceFile format -> withProgram sourceFile $ \program ->
    putStrLn $ case format of
      Compact -> programToJson program
      Pretty -> programToPrettyJson program
  Check sourceFile -> withProgram sourceFile $ \_ ->
    putStrLn (sourceFile ++ ": OK")

withProgram :: FilePath -> (Program -> IO ()) -> IO ()
withProgram sourceFile action = do
  sourceResult <- try (readFile sourceFile) :: IO (Either IOException String)
  case sourceResult of
    Left fileError -> failWith ("cannot read '" ++ sourceFile ++ "': " ++ show fileError)
    Right source -> case parseProgram source of
      Left parseError -> failWith (sourceFile ++ ": " ++ parseError)
      Right program -> action program

renderRuntimeError :: StateError -> String
renderRuntimeError runtimeError = case runtimeError of
  UndefinedVariable name -> "runtime error: undefined variable '" ++ name ++ "'"
  InputExhausted -> "runtime error: input exhausted"
  DivisionByZero -> "runtime error: division by zero"

failWith :: String -> IO a
failWith message = hPutStrLn stderr ("error: " ++ message) >> exitFailure
