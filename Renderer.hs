-- Renderer - module that maps a game state into a gloss picture

module Renderer(render) where

import State
import Graphics.Gloss

-- Let's start with rendering an empty well.

-- Playfield dimensions
cellSize = 35
padding = (768 - (20 * cellSize)) `quot` 2

wellWidth = 10 * cellSize
wellHeight = 20 * cellSize

wallWidth = wellWidth + 2 * padding
wallHeight = wellHeight + 2 * padding

-- Colors
wellColor = black
wallColor = dark (dark blue)

-- Now, before rendering the cells, we need to map from our coordinate system to the one used by gloss
-- So, let's create a mapping function to do that:

-- Convert from playfield coordinate to screen coordinate
playfieldToScreen :: (Int, Int) -> (Int, Int)
playfieldToScreen (px, py) = (sx, sy) where
  sx = (px * cellSize) `quot` 2
  sy = (11 * cellSize) + (py * cellSize) `quot` 2


render :: State -> Picture
render gameState = pictures
  [ walls
  , well
  , activePiece
  ]
  where -- using fromIntegral to convert our integers to Float that gloss requires
    walls = color wallColor (rectangleSolid (fromIntegral wallWidth) (fromIntegral wallHeight))
    well = pictures 
      [ color wellColor (rectangleSolid (fromIntegral wellWidth) (fromIntegral wellHeight))
      ]
    activePiece = pictures []
    