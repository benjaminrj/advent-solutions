module Main where

import Ticket

main :: IO ()
main = do
  putStrLn =<< ticketSolutionA
  putStrLn =<< ticketSolutionB
