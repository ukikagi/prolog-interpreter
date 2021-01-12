module Main where

import Control.Monad (mapM_, unless, when)
import Data.List (head)
import Eval (answer)
import qualified Parser
import Syntax (Prog, Subst, showSubst)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hFlush, stdout)
import System.Posix.Signals
  ( Handler (Catch),
    installHandler,
    keyboardSignal,
  )
import Text.Parsec (parse)
import Text.Parsec.Error ()
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
      Right prog ->
        do
          installHandler
            keyboardSignal
            (Catch $ putStr "\n" >> repLoop prog)
            Nothing
          repLoop prog

putStrLns :: [String] -> IO ()
putStrLns = mapM_ putStrLn

repLoop :: Prog -> IO ()
repLoop prog =
  do
    putStr "?- " >> hFlush stdout
    line <- getLine
    case parse Parser.query "" line of
      Left _ ->
        putStrLn "Error: Parse error." >> repLoop prog
      Right query ->
        printSubsts (answer prog query)
          >> repLoop prog

printSubsts :: [Subst] -> IO ()
printSubsts [] = putStrLn "false."
printSubsts sbs =
  let (sbs1, sbs2) = splitAt 10 sbs
   in do
        putStrLns $ map showSubst sbs1
        if null sbs2
          then putStr "\n"
          else getLine >> printSubsts sbs2
