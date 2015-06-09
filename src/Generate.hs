module Generate where
import System.Random
import GameState
import Rules

gen :: StdGen
gen = mkStdGen 11

randomRow :: StdGen -> ([SEntry], StdGen)
randomRow = aux [] where
    aux as g | length as == 9 = (map (Right . toEnum) as, g)
             | a `elem` as = aux as g'
             | otherwise = aux (a:as) g'
      where (a, g') = randomR (1, 9) g

randomGrid :: StdGen -> ([[SEntry]], StdGen)
randomGrid = aux [] where
    aux rs g | length rs == 9 = (rs, g)
             | otherwise = aux (r:rs) g'
      where (r, g') = randomRow g

sudokuGrid :: StdGen -> ([[SEntry]], StdGen)
sudokuGrid g = let (grid, g') = randomGrid g in
    if valid grid then (grid, g') else sudokuGrid g'
