-- Renderer - module that maps a game state into a gloss picture

module Renderer(render) where

import State
import Graphics.Gloss

render :: State -> Picture
render _ =   pictures
  [ translate (-20) (-100) $ color ballColor $ circleSolid 30 
  , translate 30 50 $ color paddleColor $ rectangleSolid 10 50
  ]
  where
    ballColor = dark green
    paddleColor = light (light blue)