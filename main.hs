import System.Environment (getArgs, getProgName)
import System.Exit
import System.IO
import System.Posix.Signals

import Data.List (head)

import Text.Parsec
import Text.Parsec.Error
import Text.Printf (printf)

import Control.Monad (when, unless, mapM_)

import qualified Parser
import Syntax
import Eval (answer)

usage :: IO ()
usage = putStrLn "Error: No input file"

main :: IO ()
main =
   do args <- getArgs
      progName <- getProgName
      when (length args /= 1) $
        printf "Usage: './%s input_file'\n" progName >> exitFailure
      str  <- readFile $ head args
      case parse Parser.prog "" str of
        Left _ -> putStrLn "Error: Parse error." >> exitFailure
        Right prog ->
           do installHandler keyboardSignal
                  (Catch $ putStr "\n" >> rep_loop prog) Nothing
              rep_loop prog

putStrLns :: [String] -> IO ()
putStrLns strs = mapM_ putStrLn strs

rep_loop :: Prog -> IO ()
rep_loop prog =
  do putStr "?- " >> hFlush stdout
     line <- getLine
     case parse Parser.query "" line of
        Left _ ->
          putStrLn "Error: Parse error." >> rep_loop prog
        Right query ->
          print_substs (answer prog query)
          >> rep_loop prog

print_substs :: [Subst] -> IO ()
print_substs [] = putStrLn "false."
print_substs sbs
  = let (sbs1, sbs2) = splitAt 10 sbs in
      do putStrLns $ map show_subst sbs1
         if (null sbs2) then putStr "\n"
         else getLine >> print_substs sbs2
