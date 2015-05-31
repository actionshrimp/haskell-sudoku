{-# LANGUAGE FlexibleContexts #-}
module Main where

import Graphics.Vty
import Control.Monad.State
import Data.Default (def)

import Render (renderState)
import Update (GameEvent(Quit), toGameEvent, updateState)
import GameState

type VtySudoku = StateT GS IO ()
mainLoop :: Vty -> VtySudoku
mainLoop vty = do
    s <- get
    liftIO $ update vty $ renderState s
    e <- lift $ nextEvent vty
    case toGameEvent e of
        Quit -> return ()
        a -> do
            modify (updateState a)
            mainLoop vty

main :: IO ()
main = do
    vty <- mkVty def
    evalStateT (mainLoop vty) def
    shutdown vty
