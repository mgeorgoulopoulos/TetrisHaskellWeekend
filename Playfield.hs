-- Playfield

module Playfield () where

import Data.List
import Piece

-- First, we start by defining the Cell type
-- This is a rectangle on-screen. For now, it can either be filled, or empty
data Cell = Empty | Full deriving (Show, Eq)

-- Then, we define a row of the playfield as a list of cells
data Row = RowOfCells [Cell] deriving (Show)

-- Creates an empty Row
emptyRow = RowOfCells (replicate 10 Empty)

-- Function stubs that tell if a row is empty or full
isRowEmpty :: Row -> Bool
isRowEmpty r = undefined

isRowFull :: Row -> Bool
isRowFull r = undefined


-- For compatibility with the Piece, our Well will consist of rows of height 2.
-- The rows in turn consist of cells of width 2.
-- There are 22 rows of 10 cells each. The top 2 rows are "invisible".
-- For simplicity, we'll put the origin (0,0) at the center of the invisible rows.
-- This way, when we render a piece at (0,0), it will be completely hidden.
data Well = WellOfRows [Row] deriving (Show)

-- Creates an empty Well
emptyWell = WellOfRows (replicate 22 emptyRow)

-- Converts Well to Ascii-art String.

-- Because I don't know yet how to differentiate between functions with the same name that exist in different modules, I just renamed the "toAA" functions:
wellToAA :: Well -> String
wellToAA (WellOfRows rs) = intercalate "\n" (map rowToString rs)
    where rowToString (RowOfCells cs) = map cellToChar cs
            where cellToChar cell | cell == Empty = '.'
                                  | otherwise     = '*'


-- Now, this is going to be a central function of our game: a function that renders a piece in a playfield.
-- We are going to use it to 'fix' fallen pieces in the Well


-- Here I'm using zip to associate every cell with a x and y in our coordinate system.

-- I'm also using the "pieceContains" function we created in Piece module

-- In order to transform the Well's cell's coordinate to Piece space, we subtract the piece coordinates (pX, pY).
-- We can then test if the piece contains a cell in that local position.

-- Finally, if either the well has a full cell in a position or the Piece has a full cell in its local position, we return a full cell.

-- I can't get over how compact the code is.

-- Also, I'm beginning to believe what they say: If your haskell program compiles, it does what you want.

renderPiece :: Piece -> (Int, Int) -> Well -> Well
renderPiece piece (pX, pY) (WellOfRows rs)
 | odd pX || odd pY = error "Odd piece coordinates used"  -- Remember that our cells are 2x2, so, starting from the origin (0,0), all coordinates must be even
 | otherwise = WellOfRows (map renderRow (zip [1,-1..(-41)] rs))
    where renderRow (y, (RowOfCells cs)) = RowOfCells (map renderCell (zip [-9,-7..9] cs))
            where renderCell (x, c) | c /= Empty                        = c
                                    | pieceContains (x-pX, y-pY) piece  = Full
                                    | otherwise                         = Empty
            
            
            
            
            
            
            
            
            
            
            
            
            