module Rules where

import GameState
import Data.List
import Data.List.Split (chunksOf)

withoutBlanks :: [SEntry] -> [SEntry]
withoutBlanks = filter (/= Left S_)

onlyOneInRow :: [SEntry] -> Bool
onlyOneInRow = all (<= 1) . map length . group . sort . withoutBlanks

onlyOneInEachRow :: [[SEntry]] -> Bool
onlyOneInEachRow = all onlyOneInRow

onlyOneInEachCol :: [[SEntry]] -> Bool
onlyOneInEachCol = all onlyOneInRow . transpose

blocks :: [[a]] -> [[a]]
blocks = map concat . concatMap (chunksOf 3 . aux) . chunksOf 3 where
    aux [a:as, b:bs, c:cs] = [a, b, c] : aux [as, bs, cs]
    aux [[], [], []] = []
    aux _ = error "unexpected pattern"

onlyOneInEachBlock :: [[SEntry]] -> Bool
onlyOneInEachBlock = all onlyOneInRow . blocks

valid :: [[SEntry]] -> Bool
valid g = onlyOneInEachRow g && onlyOneInEachCol g && onlyOneInEachBlock g
