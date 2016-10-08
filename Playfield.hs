-- Playfield

module Playfield () where

import Data.List


-- First, we start by defining the Cell type
-- This is a rectangle on-screen. For now, it can either be filled, or empty
data Cell = Empty | Full deriving (Eq)

-- Then, we define a row of the playfield as a list of cells
data Row = RowOfCells [Cell]

-- Creates an empty Row
emptyRow = RowOfCells (replicate 10 Empty)

-- Function stubs that tell if a row is empty or full
isRowEmpty :: Row -> Bool
isRowEmpty r = undefined

isRowFull :: Row -> Bool
isRowFull r = undefined


-- Finally, our playfield type that we'll call a "Well".
-- This is a list of Rows
data Well = WellOfRows [Row]

-- Creates an empty Well
emptyWell = WellOfRows (replicate 22 emptyRow)

-- Again, we need a way to visualize the playfield, so let's create a function that renders it as ascii-art:
toAA :: Well -> String
toAA (WellOfRows rs) = intercalate "\n" (map rowToString rs)
    where rowToString (RowOfCells cs) = map cellToChar cs
            where cellToChar cell | cell == Empty = '.'
                                  | otherwise     = '*'

-- That was easier than the Piece's: the mappings are 1:1 from Cell->Char . Equally elegant!