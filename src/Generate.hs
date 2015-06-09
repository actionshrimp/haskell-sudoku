module Generate where
import System.Random
import Control.Monad.Random
--import GameState
--import Rules

gen :: StdGen
gen = mkStdGen 11

randomRow :: RandomGen g => Rand g [Int]
randomRow = aux [] where
    aux as = if length as == 9 then return as else do
          a <- getRandomR (1, 9)
          aux (a:as)

--addRandomRow :: [[SEntry]] -> State StdGen [[SEntry]]
--addRandomRow [] = do 
--    g <- get
--    let (r, g') = runState randomRow g in do
--      put g'
--      return r:[]

--randomGrid :: State StdGen [SEntry]
--randomGrid = aux [] where
--    aux rs g | length rs == 9 = (rs, g)
--             | otherwise = aux (r:rs) g'
--      where (r, g') = randomRow g
--
--sudokuGrid :: StdGen -> ([[SEntry]], StdGen)
--sudokuGrid g = let (grid, g') = randomGrid g in
--    if valid grid then (grid, g') else sudokuGrid g'
