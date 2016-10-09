-- Renderer - module that maps a game state into a gloss picture

module Renderer(render) where

import State
import Graphics.Gloss

-- Let's start with rendering an empty well.

-- Playfield dimensions
cellSize = 35
padding = (768 - (20 * cellSize)) / 2

wellWidth = 10 * cellSize
wellHeight = 20 * cellSize -- We're not rendering the invisible rows

wallWidth = wellWidth + 2 * padding
wallHeight = wellHeight + 2 * padding

-- Colors
wellColor = black
wallColor = dark (dark blue)

render :: State -> Picture
render gameState =   pictures
  [ walls
  , well
  , activePiece
  ]
  where
    walls = color wallColor (rectangleSolid wallWidth wallHeight)
    well = pictures 
      [ color wellColor (rectangleSolid wellWidth wellHeight)
      ]
    activePiece = pictures []
    