{-# LANGUAGE RecordWildCards #-}
module Password where

import Data.Maybe

data PasswordRule = PasswordRule
  { requiredChar :: Char
  , minInstances :: Int
  , maxInstances :: Int
  } deriving Show

newtype Password = Password String
  deriving Show

numValidPasswords :: (PasswordRule -> Password -> Bool) -> IO String
numValidPasswords validityCheck = do
  pwAndRules <- fileToRules . lines <$> readFile "pw_input.txt"
  let numValid = length $ filter (uncurry validityCheck) pwAndRules
  return $ "There are " ++ show numValid ++ " valid passwords."
  where
    fileToRules = catMaybes . map parseToPasswordAndRule

isValid :: PasswordRule -> Password -> Bool
isValid PasswordRule{..} (Password pw) = 
  countChar >= minInstances && countChar <= maxInstances
  where countChar = length $ filter (== requiredChar) pw

isActuallyValid :: PasswordRule -> Password -> Bool
isActuallyValid PasswordRule{..} (Password pw) = 
  (== 1)
    $ length 
    $ filter (requiredChar ==)
    $ map indexLetter [minInstances, maxInstances] 
  where
    indexLetter i = (!!) pw (i - 1)

parseToPasswordAndRule :: String -> Maybe (PasswordRule, Password)
parseToPasswordAndRule xs
  | (n1, '-':ys) <- break ('-' ==) xs
  , (n2, ' ':reqChar:':':' ':pw) <- break (' ' ==) ys
    = Just (PasswordRule reqChar (toInt n1) (toInt n2), Password pw)
  | otherwise = Nothing
  where toInt = read
