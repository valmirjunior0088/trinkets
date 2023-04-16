import Error (showError)
import Intermediate.Parse (parse)
import System.Exit (die)
import System.Environment (getArgs, getProgName)

usage :: String -> String
usage name =
  "USAGE: " ++ name ++ " INPUT"

main :: IO ()
main = do
  input <- getArgs >>= \case
    [input] -> return input
    _ -> getProgName >>= die . usage

  source <- readFile input

  program <- case parse source of
    Left reason -> die (showError source reason)
    Right program -> return program
  
  print program
