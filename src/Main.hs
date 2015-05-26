{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.Except
import Data.Maybe
import UI.NCurses
import System.Environment
import Data.Text (Text, unpack)
import Text.Read (readMaybe, readEither)

data Sn = Blank | Sn Int
type SRow = [Sn]
type SGrid = [SRow]

instance Show Sn where
    show Blank = "_"
    show (Sn n) = show n

isSquare :: Int -> Bool
isSquare n = rt * rt == n
    where rt = truncate . sqrt . fromIntegral $ n

blankGrid :: Int -> SGrid
blankGrid n = (take n (repeat (take n (repeat Blank))))

cellH :: Integer
cellH = 2

cellW :: Integer
cellW = 4

drawGrid :: SGrid -> Update ()
drawGrid g = do
    drawRows 0 g

drawRows :: Integer -> SGrid -> Update ()
drawRows _ [] = return ()
drawRows i (r:rs) = do
    drawCells i 0 r
    drawRows (i + 1) rs

drawCells :: Integer -> Integer -> SRow -> Update ()
drawCells _ _ [] = return ()
drawCells i j (c:cs) = do
    drawCell i j c
    drawCells i (j + 1) cs

drawCell :: Integer -> Integer -> Sn -> Update ()
drawCell i j c = do
    moveCursor (i * cellH) (j * cellW)
    drawLineH Nothing cellW
    drawLineV Nothing cellH
    moveCursor ((i + 1) * cellH) (j * cellW)
    drawLineH Nothing cellW
    moveCursor (i * cellH) ((j + 1) * cellW)
    drawLineV Nothing cellH

liftEither :: Monad m => Either e a -> ExceptT e m a
liftEither x = ExceptT (return x)

sudokuArgs :: [String] -> Either String Int
sudokuArgs [] = Left "Please pass a grid size"
sudokuArgs (a:as) = do
    n <- case (readMaybe a :: Maybe Int) of
            Nothing -> Left "Passed arg is not a number"
            Just n -> Right n
    sn <- if (isSquare n)
            then Right n
            else Left "Passed arg is not a square number"
    return sn

sudoku :: [String] -> ExceptT e IO ()
sudoku args = do
    case (sudokuArgs args) of
        Right n -> lift $ sudokuGrid n
        Left x -> lift $ putStrLn x

-- Check out vty / vty-ui instead of ncurses?
sudokuGrid n = let g = blankGrid n in 
    runCurses $ do
        setEcho False
        w <- defaultWindow
        updateWindow w $ do
            drawGrid g
        render
        waitFor w (\ev -> ev == EventCharacter 'q'
            || ev == EventCharacter 'Q')

main :: IO ()
main = do
    args <- getArgs
    runExceptT $ ((sudoku args) :: ExceptT String IO ())
    return ()

--main :: IO ()
--main = do
--    a:args <- getArgs
--    g <- blankGrid a
--    runCurses $ do
--        setEcho False
--        w <- defaultWindow
--        updateWindow w $ do
--            case (blankGrid 9) of
--                Right g -> drawGrid g
--                Left e -> drawError e
--        render
--        waitFor w (\ev -> ev == EventCharacter 'q' || ev == EventCharacter 'Q')
--
waitFor :: Window -> (Event -> Bool) -> Curses ()
waitFor w p = loop where
    loop = do
        ev <- getEvent w Nothing
        case ev of
            Nothing -> loop
            Just ev' -> if p ev' then return () else loop
