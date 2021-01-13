module Main where

import Control.Monad (when)
import Data.List (intercalate)
import Eval (answer)
import qualified Parser
import Syntax (Prog, Subst, showSubst)
import System.Console.Haskeline (InputT, defaultSettings, getInputLine, outputStrLn, runInputT, waitForAnyKey)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import Text.Parsec (parse)
import Text.Printf (printf)

usage :: IO ()
usage = putStrLn "Error: No input file"

main :: IO ()
main =
  do
    args <- getArgs
    progName <- getProgName
    when (length args /= 1) $
      printf "Usage: './%s input_file'\n" progName >> exitFailure
    str <- readFile $ head args
    case parse Parser.prog "" str of
      Left _ -> putStrLn "Error: Parse error." >> exitFailure
      Right prog -> runInputT defaultSettings (repLoop prog)

putStrLns :: [String] -> IO ()
putStrLns = mapM_ putStrLn

repLoop :: Prog -> InputT IO ()
repLoop prog =
  do
    minput <- getInputLine "?- "
    case minput of
      Nothing -> return ()
      Just line ->
        case parse Parser.query "" line of
          Left _ ->
            outputStrLn "Error: Parse error." >> repLoop prog
          Right query ->
            printSubsts (answer prog query)
              >> repLoop prog

printSubsts :: [Subst] -> InputT IO ()
printSubsts [] = outputStrLn "false."
printSubsts sbs =
  let (sbs1, sbs2) = splitAt 10 sbs
   in do
        outputStrLn $ intercalate "\n" $ map showSubst sbs1
        if null sbs2
          then outputStrLn ""
          else do
            b <- waitForAnyKey "[Press any key to proceed or Ctrl-D to abort]"
            if b
              then printSubsts sbs2
              else outputStrLn ""
