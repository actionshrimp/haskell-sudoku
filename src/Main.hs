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
isSquare n = rt * rt == n
    where rt = truncate (sqrt x)
          x = fromIntegral n

blankGrid :: Int -> CreateGridM SGrid
blankGrid n | isSquare n = return (take n (repeat (take n (repeat Blank))))
            | otherwise = throwError IllegalGridSize

main = print "lol"
