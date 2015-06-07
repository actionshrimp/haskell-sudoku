module Render where

import Data.List.Split (chunksOf)
import Graphics.Vty
import GameState

tl :: Image
tl = string defAttr "┌"
bl :: Image
bl = string defAttr "└"
tr :: Image
tr = string defAttr "┐"
br :: Image
br = string defAttr "┘"
v :: Image
v  = string defAttr "│"
vl :: Image
vl = string defAttr "├"
vr :: Image
vr = string defAttr "┤"
h :: Image
h  = string defAttr "─"
ht :: Image
ht = string defAttr "┬"
hb :: Image
hb = string defAttr "┴"
x :: Image
x  = string defAttr "┼"
sep :: Image
sep = string defAttr " "

blockVertBorder :: Image
blockVertBorder = vertCat $ replicate 3 v

colBorderLeft :: Image
colBorderLeft = tl <-> blockVertBorder <-> vl <-> blockVertBorder <-> vl <-> blockVertBorder <-> bl

colBorderRight :: Image
colBorderRight = tr <-> blockVertBorder <-> vr <-> blockVertBorder <-> vr <-> blockVertBorder <-> br

colBorderCenter :: Image
colBorderCenter = ht <-> blockVertBorder <-> x <-> blockVertBorder <-> x <-> blockVertBorder <-> hb

bH :: Image
bH = sep <|> horizCat (replicate 3 (h <|> sep)) 

entry :: SEntry -> String
entry (Left a) = show a
entry (Right a) = show a

type RenderCell = (Bool, SEntry)
cell :: RenderCell -> Image
cell (isSelected, n) = string attrs $ entry n
    where attrs | isSelected = defAttr `withBackColor` white 
                                       `withForeColor` black
                | otherwise = defAttr

row :: (RenderCell, RenderCell, RenderCell) -> Image
row (c1, c2, c3) = sep <|> horizCat (map cellWithSep [c1, c2, c3]) where
    cellWithSep c = cell c <|> sep

block :: [(RenderCell, RenderCell, RenderCell)] -> Image
block rowBlock = vertCat (map row rowBlock)

colBlocks :: [[RenderCell]] -> Image
colBlocks [c1, c2, c3] = bH <-> vertCat (map blockAndBorder rowBlocks)
    where
        rows = zip3 c1 c2 c3
        rowBlocks = chunksOf 3 rows
        blockAndBorder rowBlock = block rowBlock <-> bH
colBlocks _ = error "Invalid cell data"

cellsForRender :: [[SEntry]] -> (Int, Int) -> [[RenderCell]]
cellsForRender cs cur = updateCell renderCells cur isSelected
    where renderCells = map (map ((,) False)) cs
          isSelected (a, b) = (not a, b)

gridFor :: GS -> Image
gridFor GS {
   cursor = cur,
   cells = cs
} = colBorderLeft <|>
                colBlocks bc1 <|> colBorderCenter <|>
                colBlocks bc2 <|> colBorderCenter <|>
                colBlocks bc3 <|> colBorderRight
    where 
        [bc1, bc2, bc3] = chunksOf 3 $ cellsForRender cs cur

space :: Image
space = string defAttr "      "

infoText :: Image
infoText = space <-> string defAttr "Sudoku! " <-> space <-> string defAttr "Press q to exit"

renderState :: GS -> Picture
renderState s = picForImage (gridFor s <|> space <|> infoText)
