{-# LANGUAGE OverloadedStrings, RankNTypes #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Plates  where

import StichPatterns
import Walls

import Data.List (partition, sortBy, elemIndex)
import qualified Data.Text as T
import Control.Monad.ST (runST, ST)
import Data.Maybe (mapMaybe)
import Data.Array.ST (STArray, readArray, writeArray, freeze, newArray)
import qualified Data.Set as S
import Data.Array (Array, assocs)
import Data.Bits ( Bits(shiftR, (.|.)) )
import Data.Word ( Word32 )
import Data.Function (on)
import qualified Data.IntMap as IM
import Data.Monoid (First(First, getFirst))
import Control.Monad (forM, guard)

import Debug.Trace (trace)
import Options.Applicative.Help (groupOrNestLine)

data Plate = Plate
  { plateTile  :: (Int, Int)
  , plateColor :: T.Text
  }

data Direction = N | E | S | W
  deriving Show

stichPatternPlating :: Config -> [Wall] -> [Plate]
stichPatternPlating cfg walls =
    -- trace ("groups: " ++ show groups) $
    -- trace ("neighbors: " ++ show neighbors) $
    -- trace ("group colors: " ++ show groupColors) $
    mapMaybe mkPlate (assocs groups)
  where
    (groups, neighbors) = runST (identifyGroups cfg walls)
    groupColors = colorGraph colors neighbors
    mkPlate (tile, group) = case IM.lookup group groupColors of
      Nothing -> Nothing -- error "inconsitent group colors map"
      Just c -> Just $ Plate tile c

colors :: [T.Text]
colors = [ "#ff0000" -- red
         , "#ffa500" -- orange
         , "#ffff00" -- yellow
         , "#008000" -- green
         , "#0000ff" -- blue
         , "#4b0082" -- indigo
         , "#ee82ee" -- violet
         ]

identifyGroups
  :: Config -> [Wall]
  -> (forall s. ST s
        ( Array (Int, Int) Int -- mapping each tile to a group ID > 0
        , IM.IntMap (S.Set Int) -- if b in map[a] then there is an edge from a to b
        ))
identifyGroups cfg walls = do
  plates <- noPlates cfg
  graph <- identifyGroups' cfg walls plates
  plates' <- freeze plates
  return (plates', graph)

noPlates :: forall s. Config -> ST s (STArray s (Int, Int) Int)
noPlates cfg = newArray ((0, 0), (gridSizeX cfg - 1, gridSizeY cfg - 1)) 0

identifyGroups'
  :: Config -> [Wall] -> STArray s (Int, Int) Int
  -> ST s (IM.IntMap (S.Set Int))
identifyGroups' cfg walls plates = go 1 (0, 0)
  where
    lookup = hasObstacleLookup walls

    go group (x, y)
      | y >= gridSizeY cfg = return IM.empty
      | otherwise = do
          label <- readArray plates (x, y)
          if label == 0 -- no group yet, identify the group
            then do
              myNeighbors <- flood cfg lookup group (x, y) plates
              allNeighbors <- go nextGroup nextCoords
              return $ IM.unionWith S.union myNeighbors allNeighbors
            else -- already labelled
              go group nextCoords

      where
        nextGroup = group + 1
        nextCoords
          | x >= gridSizeX cfg - 1 = (0    , y + 1)
          | otherwise              = (x + 1, y    )

data HasObstacleLookup = HasObstacleLookup
  { horWallsLookup :: S.Set (Int, Int)
  , verWallsLookup :: S.Set (Int, Int)
  }

hasObstacleLookup :: [Wall] -> HasObstacleLookup
hasObstacleLookup walls = HasObstacleLookup horWallsLookup verWallsLookup
  where
    horWallsLookup = S.fromList (map wallTile horWalls)
    verWallsLookup = S.fromList (map wallTile verWalls)

    (horWalls, verWalls) = partition isHor walls

    isHor (Wall _ d) = case d of
      Horizontal -> True
      Vertical   -> False

data Obstacle = NoObstacle | OuterWall | InnerWall

hasObstacle :: Config -> HasObstacleLookup -> (Int, Int) -> Direction -> Obstacle
hasObstacle cfg lookup (x, y) d
  | x < 0 = OuterWall
  | y < 0 = OuterWall
  | x >= gridSizeX cfg = OuterWall
  | y >= gridSizeY cfg = OuterWall
  | otherwise = case d of
      E -> hasObstacle cfg lookup (x + 1, y    ) W
      S -> hasObstacle cfg lookup (x    , y + 1) N
      N | y == 0 -> OuterWall
      W | x == 0 -> OuterWall
      N -> if S.member (x, y) (horWallsLookup lookup)
            then InnerWall
            else NoObstacle
      W -> if S.member (x, y) (verWallsLookup lookup)
            then InnerWall
            else NoObstacle

flood :: Config
      -> HasObstacleLookup -- lookup cache for hasObstalce function
      -> Int  -- the group we are flooding the tiles with
      -> (Int, Int) -- the tile where the flooding starts
      -> STArray s (Int, Int) Int -- The grid, labelled with 0 (no group) or group IDs > 0, modified in place
      -> ST s (IM.IntMap (S.Set Int))  -- the set of neighbouring groups
flood cfg lookup group (x, y) plates
  | x < 0 = pure IM.empty
  | y < 0 = pure IM.empty
  | x >= gridSizeX cfg = pure IM.empty
  | y >= gridSizeY cfg = pure IM.empty
  | otherwise = do
      label <- readArray plates (x, y)
      case label of
        0 -> do -- no group label yet
          writeArray plates (x, y) group
          IM.unionsWith S.union <$> sequence
            [ next N (x    , y - 1)
            , next E (x + 1, y    )
            , next S (x    , y + 1)
            , next W (x - 1, y    )
            ]

        g | g == group -> -- we have already been here
          return IM.empty

        _ -> error "flooded into different group"

      where
        next d c =
          case hasObstacle cfg lookup (x, y) d of
            NoObstacle -> flood cfg lookup group c plates
            OuterWall  -> return IM.empty
            InnerWall  -> do
              neighbor <- readArray plates c
              return $ IM.fromList
                [ (group, S.singleton neighbor)
                , (neighbor, S.singleton group)
                ]

colorGraph :: (Show a, Eq a) => [a] -> IM.IntMap (S.Set Int) -> IM.IntMap a
colorGraph colors neighbors =
    case go (IM.assocs neighbors) IM.empty of
      []  -> error "could not color graph"
      r:_ -> r
  where
    go []           r = return r
    go ((g, ns):rest) r = do
      let colors' = take (length colors) (drop g (cycle colors))
      c <- colors'
      guard $ not $ conflict c ns r
      let r' = IM.insert g c r
      go rest r'

    conflict color groupNeighbors colorAssignments =
      any (conflict' color colorAssignments) groupNeighbors

    conflict' color colorAssignments neighbor =
      case IM.lookup neighbor colorAssignments of
        Nothing -> False      -- neighbor has no color assigned yet
        Just c  -> c == color


colorize :: (Show a, Eq a) => [a] -> IM.IntMap (S.Set Int) -> IM.IntMap a
colorize colors = IM.foldrWithKey assignGroupColor IM.empty
  where
    assignGroupColor group groupNeighbors colorAssignments =
        case findGroupColor group groupNeighbors colorAssignments of
          Just colorAssignment -> IM.union colorAssignments colorAssignment
          Nothing -> trace "gap" colorAssignments -- error "unable to color groups"

    findGroupColor group groupNeighbors colorAssignments =
      getFirst $ mconcat $
        map (First . tryColor group groupNeighbors colorAssignments) colors'
      where
        colors' = take (length colors) (drop group (cycle colors))

    tryColor group groupNeighbors colorAssignments color =
        if conflict color groupNeighbors colorAssignments
          then Nothing
          else Just $ IM.singleton group color

    conflict color groupNeighbors colorAssignments =
      any (conflict' color colorAssignments) groupNeighbors

    conflict' color colorAssignments neighbor =
      case IM.lookup neighbor colorAssignments of
        Nothing -> False      -- neighbor has no color assigned yet
        Just c  -> c == color
