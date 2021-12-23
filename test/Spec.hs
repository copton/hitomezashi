{-# LANGUAGE ScopedTypeVariables #-}
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Ingredients.Basic (testsNames)

import Plates
import StichPatterns
import Walls

import TestWalls

import Data.Array
import Control.Monad.ST
import Data.Array.ST

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "plating"
    [ testFlood
    ]

config :: Config
config = Config 10 10 2 1

runWithPlates
  :: (ST s (STArray s (Int, Int) Int))
  -> ((STArray s (Int, Int) Int) -> ST s a)
  -> ((Array (Int, Int) Int), a)
runWithPlates f g = runST go

  where
    go = do
      plates <- f
      res <- g plates
      plates' <- freeze plates
      return (plates', res)

testFlood :: TestTree
testFlood = testCase "flood" $ do
    assertEqual
      "plates 0"
      expPlates0
      resPlates0
  where
    lookup = hasObstacleLookup testWalls
    (resPlates0, resGraph0) =
        runWithPlates
          (noPlates config)
          (flood config lookup 1 (0, 0))
    expPlates0 =
      listArray
        ((0, 0), (gridSizeX config -1, gridSizeY config - 1))
        (concat
          [ [1, 1, 1, 1, 0, 1, 1, 1, 1, 1]
          , [0, 1, 0, 1, 1, 1, 0, 1, 0, 1]
          , [0, 0, 0, 0, 1, 0, 0, 0, 0, 0]
          , [0 ,1 ,0, 1, 1, 1, 0, 1, 0, 1]
          , [1, 1, 1, 1, 0, 1, 1, 1, 1, 1]
          , [1, 0, 1, 0, 0, 0, 1, 0, 1, 0]
          , [0, 0, 0, 0, 0, 0, 0, 0, 0 ,0]
          , [0, 0, 0, 0, 0, 0, 0, 0, 0 ,0]
          , [0, 0, 0, 0, 0, 0, 0, 0, 0 ,0]
          , [0, 0, 0, 0, 0, 0, 0, 0, 0 ,0]
          ])


