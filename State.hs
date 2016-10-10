-- Game State

module State(State(..), initialGameState) where

import Playfield
import Piece

data State = State
    { well :: Well
    , time :: Float
    , deltaTime :: Float
    } deriving (Show)

initialGameState :: State
initialGameState = State
    { well = renderPiece tetrominoT (0, -10) emptyWell
    , time = 0
    , deltaTime = 0
    }