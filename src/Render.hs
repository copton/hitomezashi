{-# LANGUAGE OverloadedStrings #-}
module Render where

import StichPatterns
import Walls
import Plates

import Control.Monad (forM_, unless)
import Graphics.Svg
import qualified Data.Text as T

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


drawPlates :: Config -> [Plate] -> Element
drawPlates cfg ps =
    foldr (\p e -> e <> drawPlate p) mempty ps
  where
    drawPlate (Plate (x,y) c) =
      rect_ [ X_ <<- coord (tileSize cfg * x)
            , Y_ <<- coord (tileSize cfg * y)
            , Width_ <<- T.pack (show (tileSize cfg))
            , Height_ <<- T.pack (show (tileSize cfg))
            , Fill_ <<- c
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