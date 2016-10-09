-- Playfield

module Playfield (Well, emptyWell) where

import Data.List
import Piece

import Graphics.Gloss

-- A cell is a rectangle in the playfield - it can either be full or empty
data Cell = Empty | Full deriving (Show, Eq)

-- Row of cells
data Row = RowOfCells [Cell] deriving (Show)

-- Creates an empty Row
emptyRow = RowOfCells (replicate 10 Empty)

-- Function stubs that tell if a row is empty or full
isRowEmpty :: Row -> Bool
isRowEmpty r = undefined

isRowFull :: Row -> Bool
isRowFull r = undefined


-- For compatibility with the Piece, our Well consists of rows of height 2.
-- The rows in turn consist of cells of width 2.
-- There are 22 rows of 10 cells each. The top 2 rows are "invisible".
-- For simplicity, we'll put the origin (0,0) at the center of the invisible rows.
-- This way, when we render a piece at (0,0), it will be completely hidden.
data Well = WellOfRows [Row] deriving (Show)

-- Creates an empty Well
emptyWell = WellOfRows (replicate 22 emptyRow)

-- Converts Well to Ascii-art String.
wellToAA :: Well -> String
wellToAA (WellOfRows rs) = intercalate "\n" (map rowToString rs)
    where rowToString (RowOfCells cs) = map cellToChar cs
            where cellToChar cell | cell == Empty = '.'
                                  | otherwise     = '*'


-- Small refactoring: These might come in handy later.
numberRows :: Well -> [(Int, Row)]
numberRows (WellOfRows rs) = zip [1,-1..(-41)] rs

numberCells :: Row -> [(Int, Cell)]
numberCells (RowOfCells cs) = zip [-9,-7..9] cs
                                  
-- Renders a piece in the Well
renderPiece :: Piece -> (Int, Int) -> Well -> Well
renderPiece piece (pX, pY) well
 | odd pX || odd pY = error "Odd piece coordinates used"
 | otherwise = WellOfRows (map renderRow (numberRows well))
    where renderRow (y, row) = RowOfCells (map renderCell (numberCells row))
            where renderCell (x, c) 
                    | c /= Empty                        = c
                    | pieceContains (x-pX, y-pY) piece  = Full
                    | otherwise                         = Empty
            
            
            
            
            
            
            
            
            
            
            
            
            