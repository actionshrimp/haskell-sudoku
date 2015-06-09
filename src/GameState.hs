module GameState where

import Data.Default (Default, def)
import Control.Lens
import Data.Tuple (swap)
import Data.Maybe (fromJust)

data S_ = S_ deriving (Eq, Ord)
data Sn = S1 | S2 | S3 | S4 | S5 | S6 | S7 | S8 | S9 deriving (Eq, Ord)
type SEntry = Either S_ Sn
data GS = GS {
    cursor :: (Int, Int),
    cells :: [[SEntry]]
}

instance Show S_ where
    show S_ = "."

instance Show Sn where
    show S1 = "1"
    show S2 = "2"
    show S3 = "3"
    show S4 = "4"
    show S5 = "5"
    show S6 = "6"
    show S7 = "7"
    show S8 = "8"
    show S9 = "9"

instance Enum Sn where
    fromEnum = fromJust . flip lookup snTable
    toEnum = fromJust . flip lookup (map swap snTable)

snTable :: [(Sn, Int)]
snTable = [(S1, 1), (S2, 2), (S3, 3), (S4, 4), (S5, 5), (S6, 6), (S7, 7), (S8, 8), (S9, 9)]

instance Default GS where
    def = GS (0, 0) (replicate 9 (replicate 9 (Left S_)))

setCell :: [[a]] -> (Int, Int) -> a -> [[a]]
setCell cs (c, r) v = cs & element c . element r .~ v

updateCell :: [[a]] -> (Int, Int) -> (a -> a) -> [[a]]
updateCell cs (c, r) f = cs & element c . element r %~ f
