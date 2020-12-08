module Main where

import Password
import Ticket
import Trees

main :: IO ()
main = do
  putStrLn =<< ticketSolutionA
  putStrLn =<< ticketSolutionB
  putStrLn =<< numValidPasswords isValid
  putStrLn =<< numValidPasswords isActuallyValid
  treeSolution
