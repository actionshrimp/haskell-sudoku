module Rules where

import GameState
import Data.List
-- import Data.List.Split (chunksOf)

withoutBlanks :: [SEntry] -> [SEntry]
withoutBlanks = filter (/= Left S_)

onlyOneInRow :: [SEntry] -> Bool
onlyOneInRow = all (<= 1) . map length . group . sort . withoutBlanks

onlyOneInEachRow :: [[SEntry]] -> Bool
onlyOneInEachRow = all onlyOneInRow

onlyOneInEachCol :: [[SEntry]] -> Bool
onlyOneInEachCol = all onlyOneInRow . transpose

--onlyOneInEachBlock :: [[SEntry]] -> Bool
--onlyOneInEachBlock = map (chunksOf 3)

valid :: [[SEntry]] -> Bool
valid g = onlyOneInEachRow g && onlyOneInEachCol g -- && onlyOneInEachBlock g
