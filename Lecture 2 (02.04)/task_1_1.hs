{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHS -Wall #-}

import CodeWorld

data Tile = Wall | Floor | Door DoorColor  | Exit deriving (Show, Eq)
data DoorColor = Red | Blue | Green deriving (Show, Eq)
data Coords = Coords Int Int
data Dir = LeftMove | RightMove | UpMove | DownMove deriving (Show, Eq)


doorColor :: DoorColor -> Color
doorColor Red = red
doorColor Blue = blue
doorColor Green = green

drawTile :: Tile -> Picture
drawTile Wall = colored black (solidRectangle 0.95 0.95)
drawTile Floor = colored yellow (solidRectangle 0.95 0.95)
drawTile (Door dc) = colored (doorColor dc) (solidRectangle 0.95 0.95)

drawTileAt :: Coords -> Tile -> Picture
drawTileAt (Coords i j) tile =
  translated x y (drawTile tile)
  where
    x = fromIntegral i
    y = fromIntegral j

levelMap :: Coords -> Tile
levelMap (Coords i j)
  | abs i > 5 = Wall
  | abs j > 5 = Wall
  | (i, j) == (2, 3) = Wall
  | otherwise = Floor

drawFromTo :: (Int, Int) -> (Int -> Picture) -> Picture
drawFromTo (from, to) something
  | from > to = blank
  | otherwise = something from <> drawFromTo(from + 1, to) something

drawRow :: (Int, Int) -> Int -> Picture
drawRow (from, to) j = drawFromTo (from, to) (\i -> drawTileAt (Coords i j) (levelMap (Coords i j)))

drawRows :: (Int, Int) -> (Int, Int) -> Picture
drawRows (fromX, toX) (fromY, toY)
  = drawFromTo (fromY, toY) (\j -> drawRow (fromX, toX) j)

drawPlayer :: Coords -> Picture
drawPlayer (Coords i j) = translated x y  (lettering  "\x1F6B6")
   where
      x = fromIntegral i
      y = fromIntegral j

drawTileMap :: Picture
drawTileMap = drawRows (-6, 6) (-6, 6)

tryMove :: Dir -> Coords -> Coords
tryMove dir (Coords i j)
  | dir == UpMove && levelMap (Coords i (j + 1)) == Floor = (Coords i (j + 1))
  | dir == DownMove && levelMap (Coords i (j - 1)) == Floor = (Coords i (j - 1))
  | dir == LeftMove && levelMap (Coords (i - 1) j) == Floor = (Coords (i - 1) j)
  | dir == RightMove && levelMap (Coords (i + 1) j) == Floor = (Coords (i + 1) j)
  | otherwise = (Coords i j)


initialWorld :: Coords
initialWorld = Coords 0 0

handleWorld :: Event -> Coords -> Coords
handleWorld (KeyPress "Up") (Coords i j) = tryMove UpMove (Coords i j)
handleWorld (KeyPress "Down") (Coords i j) = tryMove DownMove (Coords i j)
handleWorld (KeyPress "Left") (Coords i j) = tryMove LeftMove (Coords i j)
handleWorld (KeyPress "Right") (Coords i j) = tryMove RightMove (Coords i j)
handleWorld _anyEvent coords = coords

renderWorld :: Coords -> Picture
renderWorld coords = drawPlayer coords <> drawTileMap

main :: IO ()
main = activityOf initialWorld handleWorld renderWorld
