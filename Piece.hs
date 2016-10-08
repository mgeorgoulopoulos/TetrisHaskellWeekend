-- Piece

module Piece (Piece, tetrominoI, tetrominoO, tetrominoS, tetrominoZ) where

import Data.List(intercalate)

-- A Piece will consist of a list of 2-tuples of integers
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

-- Coordinate representation isn't good for debugging.
-- So, let's create a function to convert to ascii-art string.
toAA :: Piece -> String
toAA piece = intercalate "\n" lines
    where lines = map rowToString [3,1,-1,-3]
            where rowToString row = concat (map colToString [-3,-1,1,3])
                    where colToString col | pieceContains (col, row) piece = "*"
                                          | otherwise                      = "."


