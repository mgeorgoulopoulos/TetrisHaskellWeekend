-- Logic module
-- Functions for updating game state and responding to user input

module Logic(updateGameState, handleEvent) where

import State
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game -- for Event

handleEvent :: Event -> State -> State
handleEvent _ s = s

updateGameState :: Float -> State -> State
updateGameState t s = s
