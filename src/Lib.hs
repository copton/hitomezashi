{-# LANGUAGE OverloadedStrings #-}
module Lib where

import qualified Data.Text as T
import Graphics.Svg
import System.Random ( mkStdGen, Random(randoms) )
import Data.Maybe ( catMaybes )

data Pattern = Odd | Even

data Config = Config
  { gridSizeX :: Int
  , gridSizeY :: Int
  , tileSize  :: Int
  , margin    :: Int
  }

data StichPattern = StichPattern
  { patternsX :: [Pattern]
  , patternsY :: [Pattern]
  }

data Orientation = Horizontal | Vertical

data Wall = Wall
  { wallTile        :: (Int, Int)
  , wallOrientation :: Orientation
  }


genStichPattern :: Config -> Int -> StichPattern
genStichPattern cfg seed = StichPattern
    (map label xVals)
    (map label yVals)
    where
      gen = mkStdGen seed
      stream = randoms gen :: [Bool]
      (xVals, stream') = splitAt (gridSizeX cfg) stream
      (yVals, _)       = splitAt (gridSizeY cfg) stream'

      label True  = Odd
      label False = Even

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

draw :: Config -> [Wall] -> Element
draw cfg walls = svg cfg $
    foldr (\w e -> e <> drawWall w) mempty walls
  where
    drawWall :: Wall -> Element
    drawWall (Wall tile Horizontal) =
      line_ [ X1_ <<- coord x1Hor tile
            , Y1_ <<- coord yHor tile
            , X2_ <<- coord x2Hor tile
            , Y2_ <<- coord yHor tile
            , Stroke_ <<- "black"
            ]

    drawWall (Wall tile Vertical ) =
      line_ [ X1_ <<- coord xVer tile
            , Y1_ <<- coord y1Ver tile
            , X2_ <<- coord xVer tile
            , Y2_ <<- coord y2Ver tile
            , Stroke_ <<- "black"
            ]

    coord f t = T.pack $ show $ margin cfg + f t
    x1Hor (tile, _) = tileSize cfg * tile
    x2Hor (tile, _) = tileSize cfg * (tile + 1)
    y1Ver (_, tile) = tileSize cfg * tile
    y2Ver (_, tile) = tileSize cfg * (tile + 1)
    xVer = x1Hor
    yHor = y1Ver


svg :: Config -> Element -> Element
svg config content =
     doctype
  <> with (svg11_ (canvas <> content))
      [Version_ <<- "1.1", Width_ <<- width , Height_ <<- height]

  where
    canvas :: Element
    canvas = rect_ [ Width_ <<- "100%", Height_ <<- "100%"
                   , Stroke_ <<- "black", Fill_ <<- "white"]

    width  = T.pack $ show (gridSizeX config * tileSize config + (2 * margin config))
    height = T.pack $ show (gridSizeY config * tileSize config + (2 * margin config))