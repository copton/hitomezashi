{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, RankNTypes
           , FlexibleContexts #-}
module Lib where

import Control.Monad (forM_, unless)
import Control.Monad.ST
import Data.Array (Array, assocs)
import Data.Array.ST
import Data.List (partition)
import Data.Maybe ( catMaybes )
import Graphics.Svg
import System.Random ( mkStdGen, Random(randoms) )
import qualified Data.Set as S
import qualified Data.Text as T


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
      stream = randoms gen :: [Bool]
      (xVals, stream') = splitAt (gridSizeX cfg) stream
      (yVals, _)       = splitAt (gridSizeY cfg) stream'

      label True  = Odd
      label False = Even

data Orientation = Horizontal | Vertical

data Wall = Wall
  { wallTile        :: (Int, Int)
  , wallOrientation :: Orientation
  }

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

drawWalls :: Config -> [Wall] -> Element
drawWalls cfg walls =
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

data Plate = Plate
  { plateTile  :: (Int, Int)
  }

data Direction = N | E | S | W

stichPatternPlating :: Config -> [Wall] -> [Plate]
stichPatternPlating cfg walls =
    foldr mkPlate [] (assocs plateTiles)
  where
    plateTiles :: Array (Int, Int) Bool
    plateTiles = runSTArray (colorStichPattern cfg walls)

    mkPlate (tile, True) plates = Plate tile : plates
    mkPlate _            plates = plates

colorStichPattern
  :: Config -> [Wall]
  -> (forall s. ST s (STArray s (Int, Int) Bool))
colorStichPattern cfg walls = do
  plates <- newArray ((0, 0), (maxX, maxY)) False
  flood plates (0, 0)
  -- forM_ [0..maxX] $ \x -> do
  --   forM_ [0..maxY] $ \y -> do
  --     colored <- readArray plates (x, y)
  --     case colored of
  --       Nothing -> flood plates (x, y)
  --       Just _  -> pure ()
  return plates

  where
    maxX = gridSizeX cfg
    maxY = gridSizeY cfg


    (horWalls, verWalls) = partition isHor walls

    isHor (Wall _ d) = case d of
      Horizontal -> True
      Vertical   -> False

    horWallsLookup = S.fromList (map wallTile horWalls)
    verWallsLookup = S.fromList (map wallTile verWalls)


    hasWall :: (Int, Int) -> Direction -> Bool
    hasWall (x, y) d
      | x < 0 = True
      | y < 0 = True
      | x >= maxX = True
      | y >= maxY = True
      | otherwise = case d of
          E -> hasWall (x + 1, y    ) W
          S -> hasWall (x    , y + 1) N
          N -> S.member (x, y) horWallsLookup
          W -> S.member (x, y) verWallsLookup

    flood plates (x, y)
      | x < 0 = pure ()
      | y < 0 = pure ()
      | x >= maxX = pure ()
      | y >= maxY = pure ()
      | otherwise = do
          colored <- readArray plates (x, y)
          case colored of
            True -> pure ()
            False -> do
              writeArray plates (x, y) True
              unless (hasWall (x, y) N) $ flood plates (x    , y - 1)
              unless (hasWall (x, y) E) $ flood plates (x + 1, y    )
              unless (hasWall (x, y) S) $ flood plates (x    , y + 1)
              unless (hasWall (x, y) W) $ flood plates (x - 1, y    )

drawPlates :: Config -> [Plate] -> Element
drawPlates cfg ps =
    foldr (\p e -> e <> drawPlate p) mempty ps
  where
    drawPlate (Plate (x,y)) =
      rect_ [ X_ <<- coord (tileSize cfg * x)
            , Y_ <<- coord (tileSize cfg * y)
            , Width_ <<- T.pack (show (tileSize cfg))
            , Height_ <<- T.pack (show (tileSize cfg))
            , Fill_ <<- "red"
            , Stroke_width_  <<- "0"
            ]

    coord c = T.pack $ show $ c + margin cfg

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