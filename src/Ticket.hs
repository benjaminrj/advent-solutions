module Ticket (ticketSolution) where

import System.IO
import qualified Data.HashMap.Strict as HM

ticketSolution :: IO String
ticketSolution = do
  maybeVals <- sumsTo2020 mempty  <$> fileToInts "input.txt"
  return $ case maybeVals of
    Just (a, b) -> show a ++ " times " ++ show b ++ " is " ++ show (a * b)
    Nothing -> "no solution"

fileToInts :: FilePath -> IO [Int]
fileToInts file = map read . lines <$> readFile file

sumsTo2020 :: HM.HashMap Int Int -> [Int] -> Maybe (Int, Int)
sumsTo2020 hm [] = Nothing
sumsTo2020 hm (x:xs) = case HM.lookup x hm of
  Just comp -> Just (comp, x)
  Nothing -> sumsTo2020 (HM.insert (2020 - x) x hm) xs

