-- Logic module
-- Functions for updating game state and responding to user input

module Logic(updateGameState, handleEvent) where

import State
import Piece
import Playfield
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game -- for Event
import System.Random

-- Piece falling velocity, in cells/second
pieceVelocity :: Float
pieceVelocity = 15.5

-- Time to wait before dropping piece again
piecePeriod :: Float
piecePeriod = 1.0 / pieceVelocity

handleEvent :: Event -> State -> State
handleEvent (EventKey (SpecialKey KeyLeft) Down _ _) s = movePiece (-2) s
handleEvent (EventKey (SpecialKey KeyRight) Down _ _) s = movePiece 2 s
handleEvent _ s = s

-- Moves the falling piece horizontally, if possible
movePiece :: Int -> State -> State
movePiece offset s
  | canPieceBeAt (piece s) piecePos' (well s) = s {piecePos = piecePos'}
  | otherwise = s
    where
      piecePos' = (fst (piecePos s) + offset, snd (piecePos s))

-- Update function passed to gloss
updateGameState :: Float -> State -> State
updateGameState t s = unityStyleUpdate (s {time = (time s + t), deltaTime = t}) -- ok, after all gloss passes dt to us

-- my update function
unityStyleUpdate :: State -> State
unityStyleUpdate s
  | secondsToNextMove stateWithUpdatedClocks <= 0 = applyMove stateWithUpdatedClocks {secondsToNextMove = piecePeriod}
  | otherwise                                     = stateWithUpdatedClocks
    where
      stateWithUpdatedClocks = s {secondsToNextMove = (secondsToNextMove s) - (deltaTime s)}

-- Refactored from applyMove. We also needed it to move left-right and rotate a piece
canPieceBeAt :: Piece -> (Int, Int) -> Well -> Bool
canPieceBeAt piece coord well = insidePlayfield && (not colliding)
  where
    insidePlayfield = validPos coord piece
    colliding = pieceCollides piece coord well

-- Moves the current piece one cell down
applyMove :: State -> State
applyMove s
  | nextPosInvalid    = fixPiece s 
  | otherwise         = s {piecePos = piecePos'}
    where
      nextPosInvalid = not (canPieceBeAt (piece s) piecePos' (well s))
      piecePos' = (fst (piecePos s), snd (piecePos s) - 2)

-- Fixes the falling piece to its current position and resets the piece to a new one
fixPiece :: State -> State
fixPiece s
  | ((snd (piecePos s)) > (-2)) = resetGameState s -- reset game state when 'fixing' a piece that overflows the well
  | otherwise     = s
    { well = renderPiece (piece s) (piecePos s) (well s)
    , piece = randomPiece (fst reseed)
    , piecePos = (0, 0)
    , randomSeed = snd reseed
    }
      where
        reseed :: (Double, StdGen)
        reseed = randomR (0.0, 1.0) (randomSeed s)


