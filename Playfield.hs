-- Playfield

module Playfield () where


-- First, we start by defining the Cell type
-- This is a rectangle on-screen. For now, it can either be filled, or empty
data Cell = Empty | Full deriving (Eq)

-- Then, we define a row of the playfield as a list of cells
data Row = Row [Cell]

-- Function stubs that tell if a row is empty or full
emptyRow :: Row -> Bool
emptyRow r = undefined

fullRow :: Row -> Bool
fullRow r = undefined


-- Finally, our playfield type that we'll call a "Well".
-- This is a list of Rows
data Well = Well [Row]


