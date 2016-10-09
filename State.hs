-- Game State

module State(State, well, initialGameState) where

import Playfield
import Piece

data State = State
    { well :: Well
    } deriving (Show)

initialGameState :: State
initialGameState = State
    { well = renderPiece tetrominoT (0, -10) emptyWell
    }