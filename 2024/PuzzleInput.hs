module PuzzleInput where

import Control.Exception
import Control.Monad

puzzleInput :: IO String
puzzleInput = do
  c <- catch getChar (\(ex :: IOException) -> return '\EOT')
  -- \^D -> EOT; ^Z -> SUB
  if c == '\EOT' || c == '\SUB'
    then
      return ""
    else do
      s <- puzzleInput
      return (c : s)

puzzleInputLines :: IO [String]
puzzleInputLines = fmap lines puzzleInput
