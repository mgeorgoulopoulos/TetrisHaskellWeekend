module Main(main) where

import Graphics.Gloss

import State
import Renderer

window :: Display
window = InWindow "Nice Window" (1280, 768) (200, 200)

background :: Color
background = black
    
main :: IO ()
main = display window background (render State)