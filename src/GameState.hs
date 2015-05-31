module GameState where
import Data.Default (Default, def)

data Sn = S_ | S1 | S2 | S3 | S4 | S5 | S6 | S7 | S8 | S9
data GameState = GameState {
    cursor :: (Int, Int),
    cells :: [[Sn]]
}

instance Show Sn where
    show S_ = "?"
    show S1 = "1"
    show S2 = "2"
    show S3 = "3"
    show S4 = "4"
    show S5 = "5"
    show S6 = "6"
    show S7 = "7"
    show S8 = "8"
    show S9 = "9"

instance Default GameState where
    def = GameState (0, 0) (replicate 9 (replicate 9 S_))
