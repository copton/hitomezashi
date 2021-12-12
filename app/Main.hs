module Main where

import Lib
import System.Environment (getArgs)
import Options.Applicative
import Data.Semigroup ((<>))

data Options = Options
  { optGridSizeX :: Int
  , optGridSizeY :: Int
  , optTileSize  :: Int
  , optSeed      :: Int
  }

options :: Parser Options
options = Options
  <$> option auto
        (short 'x'
        <> help "size of the grid in X direction")
  <*> option auto
        (short 'y'
        <> help "size of the grid in Y direction")
  <*> option auto
        (long "zoom"
        <> short 'z'
        <> showDefault
        <> value 10
        )
  <*> option auto
        (short 's'
        <> help "which pattern to select"
        <> showDefault
        <> value 42
        )

run :: Options -> IO ()
run options = do
  let cfg = Config
              (optGridSizeX options)
              (optGridSizeY options)
              (optTileSize options)
              ((optTileSize options) `div` 2)

  let stichPattern = genStichPattern cfg (optSeed options)

  let walls = stichPatternWalls stichPattern
  print $ draw cfg walls

main :: IO ()
main = run =<< execParser opts
  where
    opts = info (options <**> helper)
          ( fullDesc
        <> progDesc "Generate a Hitomezashi stich pattern as SVG file"
        <> header "hitomezashi - stich patterns" )
