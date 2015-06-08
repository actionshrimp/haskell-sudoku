module Generate where
import System.Random
import GameState

gen :: StdGen
gen = mkStdGen 11

mkRandomRow :: StdGen -> [Int] -> ([Int], StdGen)
mkRandomRow g as | length as == 9 = (as, g)
                 | a `elem` as = mkRandomRow g' as
                 | otherwise = mkRandomRow g' (a:as)
  where (a, g') = randomR (1, 10) g

--randomGrid :: StdGen -> ([[Int]], StdGen)
--randomGrid 
