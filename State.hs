-- Game State

module State(State(..), initialGameState, resetGameState) where

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
    , score :: Int
    } deriving (Show)

initialGameState :: State
initialGameState = State
    { well = emptyWell
    , time = 0
    , deltaTime = 0
    , secondsToNextMove = 0
    , piece = tetrominoO
    , piecePos = (0, 0)
    , randomSeed = mkStdGen 0 -- found better way!
    , score = 0
    }
    
-- Resets a game state, maintaining the random seed
resetGameState :: State -> State
resetGameState s = initialGameState {randomSeed = (randomSeed s)}