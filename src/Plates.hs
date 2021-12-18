{-# LANGUAGE OverloadedStrings, RankNTypes #-}
module Plates  where

import StichPatterns
import Walls

import Data.List (partition, sortBy, elemIndex)
import qualified Data.Text as T
import Control.Monad.ST
import Data.Maybe (fromJust)
import Data.Array.ST
import qualified Data.Set as S
import Data.Array (Array, assocs)
import Data.Bits ( Bits(shiftR, (.|.)) )
import Data.Word ( Word32 )
import Data.Function (on)
import Numeric ( showHex )

import Debug.Trace (trace)

data Plate = Plate
  { plateTile  :: (Int, Int)
  , plateColor :: T.Text
  }

data Direction = N | E | S | W

stichPatternPlating :: Config -> [Wall] -> [Plate]
stichPatternPlating cfg walls =
    foldr mkPlate [] (assocs plating)
  where
    (plating, groups) = runST (colorize cfg walls)

    groupsBySize = reverse $ map fst $ sortBy (compare `on` snd) groups

    mkPlate (tile, group) plates = case groupColor group groupsBySize of
      Nothing -> plates
      Just c ->  Plate tile c : plates

colorize
  :: Config -> [Wall]
  -> (forall s. ST s
        ( Array (Int, Int) Int
        , [(Int, Int)]
        ))
colorize cfg walls = do
  plates <- noPlates
  groups <- identifyGroups cfg walls plates
  plates' <- freeze plates
  return (plates', groups)

  where
    noPlates :: ST s (STArray s (Int, Int) Int)
    noPlates = newArray ((0, 0), (gridSizeX cfg - 1, gridSizeY cfg - 1)) 0

identifyGroups
  :: Config -> [Wall] -> STArray s (Int, Int) Int -> ST s [(Int, Int)]
identifyGroups cfg walls plates = go 1 (0, 0)
  where
    lookup = hasWallLookup walls

    go group (x, y)
      | y >= gridSizeY cfg = return []
      | otherwise = do
          val <- readArray plates (x, y)
          if val == 0
            then do
              groupSize <- flood cfg lookup plates group (x, y)
              groups <- go nextGroup nextCoords
              if groupSize == 0
                then return groups
                else return $ (group, groupSize) : groups
            else
              go group nextCoords

      where
        nextGroup = group + 1
        nextCoords
          | x >= gridSizeX cfg - 1 = (0    , y + 1)
          | otherwise              = (x + 1, y    )

data HasWallLookup = HasWallLookup
  { horWallsLookup :: S.Set (Int, Int)
  , verWallsLookup :: S.Set (Int, Int)
  }

hasWallLookup :: [Wall] -> HasWallLookup
hasWallLookup walls = HasWallLookup horWallsLookup verWallsLookup
  where
    horWallsLookup = S.fromList (map wallTile horWalls)
    verWallsLookup = S.fromList (map wallTile verWalls)

    (horWalls, verWalls) = partition isHor walls

    isHor (Wall _ d) = case d of
      Horizontal -> True
      Vertical   -> False

hasWall :: Config -> HasWallLookup -> (Int, Int) -> Direction -> Bool
hasWall cfg lookup (x, y) d
  | x < 0 = True
  | y < 0 = True
  | x >= gridSizeX cfg = True
  | y >= gridSizeY cfg = True
  | otherwise = case d of
      E -> hasWall cfg lookup (x + 1, y    ) W
      S -> hasWall cfg lookup (x    , y + 1) N
      N -> S.member (x, y) (horWallsLookup lookup)
      W -> S.member (x, y) (verWallsLookup lookup)

flood :: Config -> HasWallLookup -> STArray s (Int, Int) Int
      -> Int -> (Int, Int) -> ST s Int
flood cfg lookup plates group (x, y)
  | x < 0 = pure 0
  | y < 0 = pure 0
  | x >= gridSizeX cfg = pure 0
  | y >= gridSizeY cfg = pure 0
  | otherwise = do
      color <- readArray plates (x, y)
      case color of
        0 -> do
          writeArray plates (x, y) group
          sum <$> sequence
            [ pure 1
            , next N (x    , y - 1)
            , next E (x + 1, y    )
            , next S (x    , y + 1)
            , next W (x - 1, y    )
            ]

        g | g == group -> return 0
        _ -> error "flooded into different group"

      where
        next d c =
          if hasWall cfg lookup (x, y) d
            then
              return 0
            else
              flood cfg lookup plates group c

groupColor :: Int -> [Int] -> Maybe T.Text
groupColor group groupsBySize =
    trace (show color) $
      Just color
  where
    max :: Double
    max = fromIntegral $ length groupsBySize - 1
    scale :: Double
    scale = 255.0 * 255.0 * 255.0 / max
    idx :: Double
    idx = fromIntegral $ fromJust $ elemIndex group groupsBySize
    val :: Word32
    val = round $ idx * scale
    color :: T.Text
    color = "#" <> T.pack (showHex val "")