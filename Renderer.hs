-- Renderer - module that maps a game state into a gloss picture

module Renderer(render) where

import State
import Playfield
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
cellColor = magenta

-- Convert from playfield coordinate to screen coordinate
playfieldToScreen :: (Int, Int) -> (Int, Int)
playfieldToScreen (px, py) = (sx, sy) where
  sx = (px * cellSize) `quot` 2
  sy = (11 * cellSize) + (py * cellSize) `quot` 2

-- Function that renders a single cell
renderCell :: (Int, Int) -> Picture
renderCell (px, py) = translate (fromIntegral sx) (fromIntegral sy) (color cellColor (rectangleSolid (fromIntegral cellSize) (fromIntegral cellSize)))
  where
    sx = fst transformed
    sy = snd transformed
    transformed = playfieldToScreen (px, py)
  
-- Renders Well playfield to Picture
renderWell :: Well -> Picture
renderWell well = 
  pictures (map cellToPicture (coordCells well))
    where
      cellToPicture (px,py,c)
        | py > (-3)  = pictures []
        | c == Empty = pictures []
        | otherwise  = renderCell (px, py)
        

-- Game State renderer
render :: State -> Picture
render gameState = pictures [ walls, playfield, activePiece, debugStuff ]
  where
    walls = color wallColor (rectangleSolid (fromIntegral wallWidth) (fromIntegral wallHeight))
    playfield = pictures 
      [ color wellColor (rectangleSolid (fromIntegral wellWidth) (fromIntegral wellHeight))
      , renderWell (well gameState)
      ]
    activePiece = renderWell (renderPiece (piece gameState) (piecePos gameState) emptyWell) -- render the piece the lazy way!
    debugStuff = translate (-500.0) (200.0) (scale 0.2 0.2 (pictures [gameTime]))
      where
        gameTime = color white (Text (show (time gameState)))
    