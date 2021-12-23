module Walls where

import StichPatterns

import Data.Maybe ( catMaybes )

data Orientation = Horizontal | Vertical
  deriving Show

data Wall = Wall
  { wallTile        :: (Int, Int)
  , wallOrientation :: Orientation
  }
  deriving Show

stichPatternWalls :: StichPattern -> [Wall]
stichPatternWalls sp =
    catMaybes $ foldr iterateX [] (zip [0..] (patternsX sp))
  where
    iterateX x walls =
      foldr (iterateY x) walls (zip [0..] (patternsY sp))

    iterateY x y walls =
      wallHor x y : wallVer x y : walls

    wallHor (cx, _) (cy, py) = case py of
      Odd  | odd cx  -> Just $ Wall (cx, cy) Horizontal
      Even | even cx -> Just $ Wall (cx, cy) Horizontal
      _              -> Nothing

    wallVer (cx, px) (cy, _) = case px of
      Odd  | odd cy   -> Just $ Wall (cx, cy) Vertical
      Even | even cy -> Just $ Wall (cx, cy) Vertical
      _              -> Nothing
