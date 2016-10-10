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
pieceVelocity = 10

acceleratedPieceVelocity :: Float
acceleratedPieceVelocity = 30

effectivePieceVelocity :: State -> Float
effectivePieceVelocity s | accelerate s = acceleratedPieceVelocity | otherwise = pieceVelocity

-- Time to wait before dropping piece again
effectivePiecePeriod :: State -> Float
effectivePiecePeriod s = 1.0 / (effectivePieceVelocity s)

handleEvent :: Event -> State -> State
handleEvent (EventKey (SpecialKey KeyLeft) Down _ _) s = movePiece (-2) s
handleEvent (EventKey (SpecialKey KeyRight) Down _ _) s = movePiece 2 s
handleEvent (EventKey (SpecialKey KeyDown) Down _ _) s = s {accelerate = True}
handleEvent (EventKey (SpecialKey KeyDown) Up _ _) s = s {accelerate = False}
handleEvent (EventKey (Char 'a') Down _ _) s = rotateCW s
handleEvent (EventKey (Char 's') Down _ _) s = rotateCCW s
handleEvent _ s = s

-- Moves the falling piece horizontally, if possible
movePiece :: Int -> State -> State
movePiece offset s
  | canPieceBeAt (piece s) piecePos' (well s) = s {piecePos = piecePos'}
  | otherwise = s
    where
      piecePos' = (fst (piecePos s) + offset, snd (piecePos s))
      
-- Transforms the falling piece, if possible
transformPiece :: (Piece -> Piece) -> State -> State
transformPiece transform s
  | canPieceBeAt piece' (piecePos s) (well s) = s {piece = piece'}
  | otherwise = s
    where
      piece' = transform (piece s)

-- Rotates the falling piece clockwise, if possible
rotateCW :: State -> State
rotateCW = transformPiece pieceCW -- I feel SO badass for doing this!
rotateCCW = transformPiece pieceCCW 

-- Update function passed to gloss
updateGameState :: Float -> State -> State
updateGameState t s = unityStyleUpdate (s {time = (time s + t), deltaTime = t}) -- ok, after all gloss passes dt to us

-- my update function
unityStyleUpdate :: State -> State
unityStyleUpdate s
  | secondsToNextMove stateWithUpdatedClocks <= 0 = applyMove stateWithUpdatedClocks {secondsToNextMove = effectivePiecePeriod s}
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
  | nextPosInvalid    = handleFullRows (fixPiece s)
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
    , accelerate = False -- We don't want acceleration to affect next falling piece
    }
      where
        reseed :: (Double, StdGen)
        reseed = randomR (0.0, 1.0) (randomSeed s)


-- Removes filled rows and changes the score accordingly
handleFullRows :: State -> State
handleFullRows s = s {well = fst result, score = (score s) + linesToScore (snd result)}
  where result = clearAndCountFilledRows (well s)

-- Finally, it can't be called "Tetris" without the scoring system
linesToScore :: Int -> Int
linesToScore 0 = 0
linesToScore 1 = 40
linesToScore 2 = 100
linesToScore 3 = 300
linesToScore 4 = 1200
linesToScore _ = error "Invalid cleared Line count"