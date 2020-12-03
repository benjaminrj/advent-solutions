module Main where

import Ticket

main :: IO ()
main = do
  ints <- concatMap show <$> fileToInts "input.txt"
  putStrLn ints
