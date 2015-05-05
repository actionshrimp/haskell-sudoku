module Main where

import Control.Monad.Except
import UI.NCurses

data Sn = Blank | Sn Int
type SRow = [Sn]
type SGrid = [SRow]

instance Show Sn where
    show Blank = "_"
    show (Sn n) = show n

data CreateGridError = IllegalGridSize
instance Show CreateGridError where
    show IllegalGridSize = "Illegal grid size!"

isSquare :: Int -> Bool
isSquare n = rt * rt == n
    where rt = truncate . sqrt . fromIntegral $ n

blankGrid :: Int -> Either CreateGridError SGrid
blankGrid n | isSquare n = return (take n (repeat (take n (repeat Blank))))
            | otherwise = throwError IllegalGridSize

drawGrid :: SGrid -> Update ()
drawGrid [] = return ()
drawGrid (r:rs) = do
    drawRow (length r) r
    drawGrid rs

drawRow l (n:ns) = do
    drawString "_"

drawError :: CreateGridError -> Update ()
drawError e = do
    drawString $ show e

main :: IO ()
main = runCurses $ do
    setEcho False
    w <- defaultWindow
    updateWindow w $ do
        moveCursor 1 10
        blankGrid 9 `catchError` drawError
    render
    waitFor w (\ev -> ev == EventCharacter 'q' || ev == EventCharacter 'Q')

waitFor :: Window -> (Event -> Bool) -> Curses ()
waitFor w p = loop where
    loop = do
        ev <- getEvent w Nothing
        case ev of
            Nothing -> loop
            Just ev' -> if p ev' then return () else loop

