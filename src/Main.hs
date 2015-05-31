{-# LANGUAGE FlexibleContexts #-}
module Main where

import Graphics.Vty
import Control.Monad.State
import Data.Default (def)

import Grid (gridFor)
import GameState

eventBufferSize :: Int
eventBufferSize = 5

space :: Image
space = string defAttr "      "
infoText :: Image
infoText = space <-> string defAttr "Sudoku! " <-> space <-> string defAttr "Press q to exit"

render :: GameState -> Picture
render s = picForImage (gridFor (cells s) <|> space <|> infoText)

type VtySudoku = StateT GameState IO ()
mainLoop :: Vty -> VtySudoku
mainLoop vty = do
    s <- get
    liftIO $ update vty $ render s
    e <- lift $ nextEvent vty
    case e of
        EvKey (KChar 'q') _ -> return ()
        _ -> mainLoop vty

main :: IO ()
main = do
    vty <- mkVty def
    evalStateT (mainLoop vty) def
    shutdown vty
