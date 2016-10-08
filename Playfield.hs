-- Playfield

module Playfield () where


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

-- Our playfield will be stored differently, partially as an exercise and partially to be able to store the rendered pieces with different colours in the playfield
