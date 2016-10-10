-- Game State

module State(State(..), initialGameState) where

import Playfield
import Piece
import System.Random

data State = State
    { well :: Well
    , time :: Float
    , deltaTime :: Float
    , secondsToNextMove :: Float
    , piece :: Piece
    , piecePos :: (Int, Int)
    , randomSeed :: StdGen
    } deriving (Show)

initialGameState :: StdGen -> State
initialGameState seed = State
    { well = emptyWell
    , time = 0
    , deltaTime = 0
    , secondsToNextMove = 0
    , piece = tetrominoO
    , piecePos = (0, 0)
    , randomSeed = seed
    }