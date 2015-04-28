module Main where

import Control.Monad.Except

data Sn = Blank | Sn Int
type SRow = [Sn]
type SGrid = [SRow]

data CreateGridError = IllegalGridSize
instance Show CreateGridError where
    show IllegalGridSize = "Illegal grid size!"

type CreateGridM = Either CreateGridError

isSquare :: Int -> Bool
isSquare 1 = True
isSquare 4 = True
isSquare 9 = True
isSquare 16 = True
isSquare _ = False

blankGrid :: Int -> CreateGridM SGrid
blankGrid n | isSquare n = return (take n (repeat (take n (repeat Blank))))
            | otherwise = throwError IllegalGridSize

main = print "lol"
