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
    validPos,
    randomPiece
    , pieceColor
    ) where

import Data.List(intercalate)
import Graphics.Gloss

-- A Piece will consist of a list of 2-tuples of integers.
-- The elements of the tuple are x and y coordinates.
-- There can be negative coordinates and the center of rotation will be (0, 0)
-- Each cell will have a width and height of '2' in this coordinate system.
-- This way, the cell (1, 0) and (-1, 0) will be adjacent (distance is 1 - (-1) = 2
data Piece = PieceCoords [(Int, Int)] Color deriving (Show)

-- Piece definitions
tetrominoI = PieceCoords [(-3, -1), (-1, -1), (1, -1), (3, -1)] (dark cyan)
tetrominoO = PieceCoords [(-1, -1), (1, -1), (-1, 1), (1, 1)] (dark yellow)
tetrominoS = PieceCoords [(-1, -1), (1, -1), (1, 1), (3,1)] (dark green)
tetrominoZ = PieceCoords [(-1, -1), (1, -1), (-3,1), (-1, 1)] red
tetrominoT = PieceCoords [(-1, -1), (1, -1), (3,-1), (1, 1)] (dark magenta)
tetrominoJ = PieceCoords [(-1, -1), (1, -1), (3,-1), (-1, 1)] blue
tetrominoL = PieceCoords [(-3,-1),(-1, -1), (1, -1), (1, 1)] (dark orange)

-- Checks if a piece contains the given coordinate
pieceContains :: (Int, Int) -> Piece -> Bool
pieceContains c (PieceCoords cs _) = elem c cs

-- Converts a piece to an ascii-art string
pieceToAA :: Piece -> String
pieceToAA piece = intercalate "\n" lines
    where lines = map rowToString [3,1,-1,-3]
            where rowToString row = concat (map colToString [-3,-1,1,3])
                    where colToString col | pieceContains (col, row) piece = "*"
                                          | otherwise                      = "."

-- Piece clockwise rotation
pieceCW :: Piece -> Piece
pieceCW (PieceCoords cs col) = PieceCoords (map rotateCW cs) col
    where rotateCW (a,b) = (b,-a)

-- Piece counter-clockwise rotation
pieceCCW :: Piece -> Piece
pieceCCW (PieceCoords cs col) = PieceCoords (map rotateCCW cs) col
    where rotateCCW (a,b) = (-b,a)
    
-- Checks if a position is valid for a piece with respect to well's bounds
validPos :: (Int, Int) -> Piece -> Bool
validPos (x, y) (PieceCoords cs _) = and (map validCoord cs)
  where validCoord (px, py) = 
           (px + x >= -9) && (px + x <= 9) &&
           (py + y <= 1) && (py + y >= -41)

-- Converts random integer to piece
randomPiece :: Double -> Piece
randomPiece r = case ((truncate(r * 1000)) `mod` 7) of -- I only found randomR in stackOverflow and I won't be bothered to search for integer randms. This is a tiny part of a part of what I want to do here. I should have been able to find something in the first 3 google results or so.
  0 -> tetrominoI
  1 -> tetrominoO
  2 -> tetrominoS
  3 -> tetrominoZ
  4 -> tetrominoT
  5 -> tetrominoJ
  6 -> tetrominoL

-- returns the color of the piece
pieceColor :: Piece -> Color
pieceColor (PieceCoords _ col) = col