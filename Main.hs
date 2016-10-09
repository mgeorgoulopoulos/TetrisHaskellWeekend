module Main(main) where

import Graphics.Gloss

import State
import Renderer
import Logic

window :: Display
window = InWindow "Nice Window" (1280, 768) (200, 200)

background :: Color
background = black
fps = 60
    
main :: IO ()
main = play window background fps initialGameState render handleEvent updateGameState