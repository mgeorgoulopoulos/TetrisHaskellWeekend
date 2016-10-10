-- Tetris Piece module

module Piece
    (
    Piece, 
    pieceContains,
    tetrominoI, tetrominoO, tetrominoS, tetrominoZ,
    tetrominoT, tetrominoJ, tetrominoL,
    pieceToAA,
    pieceCW,
    pieceCCW,
    validPos
    ) where

import Data.List(intercalate)

-- A Piece will consist of a list of 2-tuples of integers.
-- The elements of the tuple are x and y coordinates.
-- There can be negative coordinates and the center of rotation will be (0, 0)
-- Each cell will have a width and height of '2' in this coordinate system.
-- This way, the cell (1, 0) and (-1, 0) will be adjacent (distance is 1 - (-1) = 2
data Piece = PieceCoords [(Int, Int)] deriving (Show)

-- Piece definitions
tetrominoI = PieceCoords [(-3, -1), (-1, -1), (1, -1), (3, -1)]
tetrominoO = PieceCoords [(-1, -1), (1, -1), (-1, 1), (1, 1)]
tetrominoS = PieceCoords [(-1, -1), (1, -1), (1, 1), (3,1)]
tetrominoZ = PieceCoords [(-1, -1), (1, -1), (-3,1), (-1, 1)]
tetrominoT = PieceCoords [(-1, -1), (1, -1), (3,-1), (1, 1)]
tetrominoJ = PieceCoords [(-1, -1), (1, -1), (3,-1), (-1, 1)]
tetrominoL = PieceCoords [(-3,-1),(-1, -1), (1, -1), (1, 1)]

-- Checks if a piece contains the given coordinate
pieceContains :: (Int, Int) -> Piece -> Bool
pieceContains c (PieceCoords cs) = elem c cs

-- Converts a piece to an ascii-art string
pieceToAA :: Piece -> String
pieceToAA piece = intercalate "\n" lines
    where lines = map rowToString [3,1,-1,-3]
            where rowToString row = concat (map colToString [-3,-1,1,3])
                    where colToString col | pieceContains (col, row) piece = "*"
                                          | otherwise                      = "."

-- Piece clockwise rotation
pieceCW :: Piece -> Piece
pieceCW (PieceCoords cs) = PieceCoords (map rotateCW cs)
    where rotateCW (a,b) = (b,-a)

-- Piece counter-clockwise rotation
pieceCCW :: Piece -> Piece
pieceCCW (PieceCoords cs) = PieceCoords (map rotateCCW cs)
    where rotateCCW (a,b) = (-b,a)
    
-- Checks if a position is valid for a piece with respect to well's bounds
validPos :: (Int, Int) -> Piece -> Bool
validPos (x, y) (PieceCoords cs) = and (map validCoord cs)
  where validCoord (px, py) = 
           (px + x >= -9) && (px + x <= 9) &&
           (py + y <= 1) && (py + y >= -41)






