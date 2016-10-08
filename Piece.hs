-- Piece

module Piece (Piece, tetrominoI) where

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


