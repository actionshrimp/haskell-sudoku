module Update where

import Graphics.Vty
import GameState
import Rules (valid)
import Generate

data GameEvent = Quit | CurLeft | CurRight | CurUp | CurDown | PutSn SEntry | DoNothing | GenGrid

toGameEvent :: Event -> GameEvent
toGameEvent (EvKey (KChar 'q') []) = Quit
toGameEvent (EvKey KEsc []) = Quit
toGameEvent (EvKey KLeft []) = CurLeft
toGameEvent (EvKey KRight []) = CurRight
toGameEvent (EvKey KUp []) = CurUp
toGameEvent (EvKey KDown []) = CurDown
toGameEvent (EvKey KBS []) = PutSn (Left S_)
toGameEvent (EvKey KDel []) = PutSn (Left S_)
toGameEvent (EvKey (KChar '1') []) = PutSn (Right S1)
toGameEvent (EvKey (KChar '2') []) = PutSn (Right S2)
toGameEvent (EvKey (KChar '3') []) = PutSn (Right S3)
toGameEvent (EvKey (KChar '4') []) = PutSn (Right S4)
toGameEvent (EvKey (KChar '5') []) = PutSn (Right S5)
toGameEvent (EvKey (KChar '6') []) = PutSn (Right S6)
toGameEvent (EvKey (KChar '7') []) = PutSn (Right S7)
toGameEvent (EvKey (KChar '8') []) = PutSn (Right S8)
toGameEvent (EvKey (KChar '9') []) = PutSn (Right S9)
toGameEvent (EvKey (KChar 'g') []) = GenGrid
toGameEvent _ = DoNothing

updateState :: GameEvent -> GS -> GS
updateState CurLeft s@(GS { cursor=(c, r) }) = s { cursor=((c - 1) `mod` 9, r)}
updateState CurRight s@(GS { cursor=(c, r) }) = s { cursor=((c + 1) `mod` 9, r)}
updateState CurUp s@(GS { cursor=(c, r) }) = s { cursor=(c, (r - 1) `mod` 9)}
updateState CurDown s@(GS { cursor=(c, r) }) = s { cursor=(c, (r + 1) `mod` 9)}
updateState (PutSn a) s@(GS { cells=cs, cursor=cur }) = s {
    cells = let updated = setCell cs cur a in
        if valid updated then updated else cs
}
--updateState GenGrid s@(GS { seed=seed }) = s {
--    cells = head $ [
--}

updateState _ s = s
