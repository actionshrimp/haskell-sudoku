module Generate where
import System.Random
import Control.Monad.Random
import Data.List.Split (chunksOf)
import Control.Monad (foldM)
--import GameState
--import Rules

gen :: StdGen
gen = mkStdGen 11

block :: [[Int]] -> (Int, Int) -> [Int]
block grid (r, c) = let
    bR = r `div` 3
    bC = c `div` 3
    bRow = take 3 . drop (3 * bR) $ grid
    in
      concatMap (take 3 . drop (3 * bC)) bRow

allowedFor :: [Int] -> (Int, Int) -> [Int]
allowedFor g (r, c) = filter notUsed [1..9] where
    grid = chunksOf 9 g
    row | r >= (length grid - 1) = []
        | otherwise = grid !! r
    col = map (!! c) grid 
    bl = block grid (r, c)
    notUsed x = x `notElem` row && x `notElem` col && x `notElem` bl

randomAllowed :: RandomGen g => [Int] -> (Int, Int) -> Rand g Int
randomAllowed g c = fromList $ map (\x -> (x, 1)) $ allowedFor g c

randomGrid :: Rand g [Int]
randomGrid = let
    coords = concat [[(r, c) | c <- [0..8]] | r <- [0..8]]
    in foldM (\acc x -> randomAllowed acc x) : acc) [] coords

--sudokuGrid :: StdGen -> ([[SEntry]], StdGen)
--sudokuGrid g = let (grid, g') = randomGrid g in
--    if valid grid then (grid, g') else sudokuGrid g'
