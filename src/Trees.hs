{-# LANGUAGE RecordWildCards #-}
module Trees where

data Movement = Movement
  { xCoord :: Int
  , yCoord :: Int 
  , xUpdate :: Int
  , yUpdate :: Int
  } deriving Show

treeSolution :: IO ()
treeSolution = do
  treesOnPath <- sequence
    [ countTrees m 0 <$> treeLines | m <- movements ]
  mapM_ sayTrees treesOnPath
  putStrLn
    $ "gets you: " ++ show (product treesOnPath)
  where
    sayTrees = putStrLn . (++ " trees times") . show
    def = Movement 0 0 0 0
    movements =
      [ def{ xUpdate = 1, yUpdate = 1 }
      , def{ xUpdate = 3, yUpdate = 1 }
      , def{ xUpdate = 5, yUpdate = 1 }
      , def{ xUpdate = 7, yUpdate = 1 }
      , def{ xUpdate = 1, yUpdate = 2 }
      ]

treeLines :: IO [String]
treeLines = lines <$> readFile "trees_input.txt"

countTrees :: Movement -> Int -> [String] -> Int
countTrees m@Movement{..} currentCount tLines =
  case tLines of
    (t:ts) ->
      countTrees
        -- Update x for next y.
        m{ xCoord = xCoord + xUpdate }
        -- Check if x is a tree, add to counter.
        (currentCount + countTree t)
        -- Move to next y.
        (drop (yUpdate - 1) ts)
    [] -> currentCount
  where
    countTree t
      | t !! (xCoord `mod` length t) == '#' = 1
      | otherwise = 0
