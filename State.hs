-- Game State

module State(State(..), initialGameState) where

import Playfield
import Piece

data State = State
    { well :: Well
    , time :: Float
    , deltaTime :: Float
    , secondsToNextMove :: Float
    , piece :: Piece
    , piecePos :: (Int, Int)
    } deriving (Show)

initialGameState :: State
initialGameState = State
    { well = renderPiece tetrominoT (0, -40) emptyWell
    , time = 0
    , deltaTime = 0
    , secondsToNextMove = 0
    , piece = tetrominoO
    , piecePos = (0, 0)
    }