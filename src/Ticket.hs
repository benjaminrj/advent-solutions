module Ticket (fileToInts) where

import System.IO

solution :: IO String
solution = do
  maybeVals <- sumsTo2020 <$> fileToInts "input.txt"
  return $ case maybeVals of
    Just (a, b) -> show a ++ " times " ++ show b ++ " is " ++ show (a * b)
    Nothing -> "no solution"

fileToInts :: FilePath -> IO [Int]
fileToInts file = map read . lines <$> readFile file

sumsTo2020 :: [Int] -> Maybe (Int, Int)
sumsTo2020 ints = case ints of
  (a:b:xs)
    | Just vals <- checkAgainstEvery a (b:xs) -> Just vals
    | otherwise -> sumsTo2020 (b:xs)
  _ -> Nothing
  where
    checkAgainstEvery checkInt (x:xs)
      | checkInt + x == 2020 = Just (checkInt, x)
      | otherwise = checkAgainstEvery checkInt xs
    checkAgainstEvery _ _ = Nothing
