import Error (showError)
import Intermediate.Parse (parse)
import Intermediate.Compile (compile)
import WebAssembly.Serialize (writeModule)
import System.Exit (die)
import System.Environment (getArgs, getProgName)

usage :: String -> String
usage name =
  "USAGE: " ++ name ++ " INPUT OUTPUT"

main :: IO ()
main = do
  (input, output) <- getArgs >>= \case
    [input, output] -> return (input, output)
    _ -> getProgName >>= die . usage

  source <- readFile input

  program <- case parse source of
    Left reason -> die (showError source reason)
    Right program -> return program

  writeModule output (compile program)
