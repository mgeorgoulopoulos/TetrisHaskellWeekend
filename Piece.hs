-- Piece

module Piece (Piece, tetrominoI) where

import Data.List(intercalate)

-- A Piece will consist of a list of 2-tuples of integers
-- The elements of the tuple are x and y coordinates.
-- There can be negative coordinates and the center of rotation will be (0, 0)
-- Each cell will have a width and height of '2' in this coordinate system.
-- This way, the cell (1, 0) and (-1, 0) will be adjacent (distance is 1 - (-1) = 2
data Piece = PieceCoords [(Int, Int)] deriving (Show)

--    Tetromino: I
--   --------------
--    -3 -1 +1 +3
--   +--+--+--+--+
-- +1|  |  |  |  |
--   +--+--0--+--+
-- -1|XX|XX|XX|XX|
--   +--+--+--+--+

tetrominoI = PieceCoords [(-3, -1), (-1, -1), (1, -1), (3, -1)]

-- Checks if a piece contains the given coordinate
pieceContains :: (Int, Int) -> Piece -> Bool
pieceContains c (PieceCoords cs) = elem c cs

-- Coordinate representation isn't good for debugging.
-- So, let's create a function to convert to ascii-art string.
toAA :: Piece -> String
toAA piece = intercalate "\n" lines
    where lines = map rowToString [3,1,-1,-3]
            where rowToString row = concat (map colToString [-3,-1,1,3])
                    where colToString col | row == (-1) = "*"
                                          | otherwise = "'"
        
-- This is the same but we are now working in the cell level
-- It is obvious how to proceed














