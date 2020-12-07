module Trees where

treeSolution :: IO String
treeSolution = do
  treesOnPath <- countTrees 0 0 <$> treeLines
  return $ (show treesOnPath) ++ " trees."

treeLines :: IO [String]
treeLines = lines <$> readFile "trees_input.txt"

countTrees :: Int -> Int -> [String] -> Int
countTrees index currentCount tLines =
  case tLines of
    (t:ts) -> countTrees (index + 3) (currentCount + countTree t) ts
    [] -> currentCount
  where
    countTree t
      | t !! (index `mod` length t) == '#' = 1
      | otherwise = 0
