-- Playfield

module Playfield
  ( Well
  , Cell(..)
  , emptyWell
  , numberRows
  , numberCells
  , coordCells
  , renderPiece
  , pieceCollides
  , clearAndCountFilledRows
  ) where

import Piece
import Data.List
import Graphics.Gloss

-- A cell is a rectangle in the playfield - it can either be full or empty
data Cell = Empty | FilledWith Color deriving (Show, Eq)

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
                                  
-- This one will create a 3-tuple of the cell and its coordinates (x,y,cell)
-- It's going to be more convenient for the rendering module to just use these 3-tuples instead of manually unfolding the well
coordCells :: Well -> [(Int, Int, Cell)]
coordCells w = concat (map extractCells (numberRows w))
  where
    extractCells (y, cs) = map extractCell (numberCells cs)
      where extractCell (x, c) = (x, y, c)
                                  
-- Renders a piece in the Well
renderPiece :: Piece -> (Int, Int) -> Well -> Well
renderPiece piece (pX, pY) well
 | odd pX || odd pY = error "Odd piece coordinates used"
 | otherwise = WellOfRows (map renderRow (numberRows well))
    where renderRow (y, row) = RowOfCells (map renderCell (numberCells row))
            where renderCell (x, c) 
                    | c /= Empty                        = c
                    | pieceContains (x-pX, y-pY) piece  = FilledWith (pieceColor piece)
                    | otherwise                         = Empty
            
-- Because I'm extremely lazy, I'm going to implement collision detection based on the renderPiece function I have already
-- First, I'll render a piece in an enpty well
-- Then, all that is needed is a function that compares two wells cell by cell
pieceCollides :: Piece -> (Int, Int) -> Well -> Bool
pieceCollides piece piecePos well = wellsCollide rendered well
  where
    rendered = renderPiece piece piecePos emptyWell
    wellsCollide (WellOfRows rs1) (WellOfRows rs2) = or (map rowsCollide (zip rs1 rs2))
    rowsCollide (RowOfCells cs1, RowOfCells cs2) = or (map cellsCollide (zip cs1 cs2))
    cellsCollide (a, b) = (a /= Empty) && (b /= Empty)
            
            
-- Clears a well of its filled rows and returns the filled row count
clearAndCountFilledRows :: Well -> (Well, Int)
clearAndCountFilledRows (WellOfRows rs) = (well', count)
  where
    well' = WellOfRows (asManyClear ++ remaining)
    remaining = filter notFull rs
    count :: Int
    count = (length rs) - (length remaining)
    asManyClear :: [Row]
    asManyClear = replicate count emptyRow
    notFull (RowOfCells cs) = not (and (map (\c -> c /= Empty) cs))
            
            
            
            
            
            
            
            