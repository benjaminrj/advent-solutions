module Ticket (ticketSolutionA, ticketSolutionB) where

import System.IO
import qualified Data.HashMap.Strict as HM

ticketSolutionA :: IO String
ticketSolutionA = do
  maybeVals <- sumsTo 2020 mempty <$> fileToInts "input.txt"
  return $ case maybeVals of
    Just (a, b) -> show a ++ " times " ++ show b ++ " is " ++ show (a * b)
    Nothing -> "no solution"

ticketSolutionB :: IO String
ticketSolutionB = do
  maybeVals <- sums3To 2020 [] <$> fileToInts "input.txt"
  return $ case maybeVals of
    Just (a, b, c)
      -> show a ++ " times " 
      ++ show b ++ " times "
      ++ show c ++ " is " 
      ++ show (a * b * c)
    Nothing -> "no solution"

fileToInts :: FilePath -> IO [Int]
fileToInts file = map read . lines <$> readFile file

sumsTo :: Int -> HM.HashMap Int Int -> [Int] -> Maybe (Int, Int)
sumsTo _ _ [] = Nothing
sumsTo s hm (x:xs)
  | Just comp <- HM.lookup x hm = Just (comp, x)
  | otherwise = sumsTo s (HM.insert (s - x) x hm) xs

-- Whatever, just tacking a loop onto 2Sum.
sums3To :: Int -> [Int] -> [Int] -> Maybe (Int, Int, Int)
sums3To _ _ [] = Nothing
sums3To s checkedList (x:xs)
  | Just (v1, v2) <- sumsTo (s - x) mempty (checkedList ++ xs)
    = Just (x, v1, v2)
  | otherwise = sums3To s (x : checkedList) xs
