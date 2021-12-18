module StichPatterns where

import System.Random ( mkStdGen, Random(randoms) )

data Pattern = Odd | Even

data Config = Config
  { gridSizeX :: Int
  , gridSizeY :: Int
  , tileSize  :: Int
  , margin    :: Int
  }
  deriving Show

data StichPattern = StichPattern
  { patternsX :: [Pattern]
  , patternsY :: [Pattern]
  }

genStichPattern :: Config -> Int -> StichPattern
genStichPattern cfg seed = StichPattern
    (map label xVals)
    (map label yVals)
    where
      gen = mkStdGen seed

      patterns = randoms gen :: [Bool]
      (xVals, patterns') = splitAt (gridSizeX cfg) patterns
      (yVals, _)       = splitAt (gridSizeY cfg) patterns'

      label True  = Odd
      label False = Even