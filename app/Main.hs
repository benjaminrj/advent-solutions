module Main where

import Password
import Ticket

main :: IO ()
main = do
  putStrLn =<< ticketSolutionA
  putStrLn =<< ticketSolutionB
  putStrLn =<< numValidPasswords
