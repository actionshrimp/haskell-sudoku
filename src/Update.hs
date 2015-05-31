module Update where

import Graphics.Vty
import GameState

data GameEvent = Quit | CurLeft | CurRight | CurUp | CurDown | PutSn Sn | DoNothing

toGameEvent :: Event -> GameEvent
toGameEvent (EvKey (KChar 'q') []) = Quit
toGameEvent (EvKey KEsc []) = Quit
toGameEvent (EvKey KLeft []) = CurLeft
toGameEvent (EvKey KRight []) = CurRight
toGameEvent (EvKey KUp []) = CurUp
toGameEvent (EvKey KDown []) = CurDown
toGameEvent (EvKey KBS []) = PutSn S_
toGameEvent (EvKey KDel []) = PutSn S_
toGameEvent (EvKey (KChar '1') []) = PutSn S1
toGameEvent (EvKey (KChar '2') []) = PutSn S2
toGameEvent (EvKey (KChar '3') []) = PutSn S3
toGameEvent (EvKey (KChar '4') []) = PutSn S4
toGameEvent (EvKey (KChar '5') []) = PutSn S5
toGameEvent (EvKey (KChar '6') []) = PutSn S6
toGameEvent (EvKey (KChar '7') []) = PutSn S7
toGameEvent (EvKey (KChar '8') []) = PutSn S8
toGameEvent (EvKey (KChar '9') []) = PutSn S9
toGameEvent _ = DoNothing

updateState :: GameEvent -> GS -> GS
updateState CurLeft s@(GS { cursor=(c, r) }) = s { cursor=((c - 1) `mod` 9, r)}
updateState CurRight s@(GS { cursor=(c, r) }) = s { cursor=((c + 1) `mod` 9, r)}
updateState CurUp s@(GS { cursor=(c, r) }) = s { cursor=(c, (r - 1) `mod` 9)}
updateState CurDown s@(GS { cursor=(c, r) }) = s { cursor=(c, (r + 1) `mod` 9)}
updateState (PutSn a) s@(GS { cells=cs, cursor=cur }) = s {
    cells=setCell cs cur a
}
updateState _ s = s
