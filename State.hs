-- Game State

module State(State, initialGameState) where

import Playfield

data State = State
    { well :: Well
    } deriving (Show)

initialGameState :: State
initialGameState = State
	{ well = emptyWell	
	}