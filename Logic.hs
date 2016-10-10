-- Logic module
-- Functions for updating game state and responding to user input

module Logic(updateGameState, handleEvent) where

import State
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game -- for Event

handleEvent :: Event -> State -> State
handleEvent _ s = s

-- I want a unity style with time and deltaTime in the state struct.
-- So I'm just going to diff the times here and do the update in another func
updateGameState :: Float -> State -> State
updateGameState t s = unityStyleUpdate (s {time = t, deltaTime = t - (time s)})

unityStyleUpdate :: State -> State
unityStyleUpdate s = s
