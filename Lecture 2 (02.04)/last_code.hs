{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHS -Wall #-}

import CodeWorld

data Tile = Wall | Floor | Door DoorColor  | Exit  | Button deriving (Show, Eq)
data DoorColor = Red | Blue | Green deriving (Show, Eq)
data Coords = Coords Int Int
data Dir = LeftMove | RightMove | UpMove | DownMove deriving (Show, Eq)

levelMap :: Coords -> Tile
levelMap (Coords i j)
  | (i, j) == (5, 3) = Door Red
  | j == 3 = Wall
  | i > 10 = Wall
  | i < -9 = Wall
  | j > 10 = Wall
  | j < -9 = Wall
  | (i, j) == (2, 3) = Wall
  | (i, j) == (2, 2) = Button
  | (i, j) == (3, 2) = Exit
  | otherwise = Floor

doorColor :: DoorColor -> Color
doorColor Red = red
doorColor Blue = blue
doorColor Green = green

drawTile :: Tile -> Picture
drawTile Wall = colored black (solidRectangle 0.95 0.95)
drawTile Floor = colored yellow (solidRectangle 0.95 0.95)
drawTile Button = colored red (solidCircle 0.45) <> colored yellow (solidRectangle 0.95 0.95)
drawTile Exit = colored green (solidRectangle 0.95 0.95)
drawTile (Door dc) = colored (doorColor dc) (solidCircle 0.45) <> colored black (solidRectangle 0.95 0.95)

drawTileAt :: Coords -> Tile -> Picture
drawTileAt (Coords i j) tile =
  translated x y (drawTile tile)
  where
    x = fromIntegral i
    y = fromIntegral j

drawRow :: (Coords -> Tile) -> (Int, Int) -> Int -> Picture
drawRow mapToDraw (from, to) j = drawFromTo (from, to) (\i -> drawTileAt (Coords i j) (mapToDraw (Coords i j)))

drawFromTo :: (Int, Int) -> (Int -> Picture) -> Picture
drawFromTo (from, to) something
  | from > to = blank
  | otherwise = something from <> drawFromTo(from + 1, to) something

drawPlayer :: Coords -> Picture
drawPlayer (Coords i j) = translated x y  (lettering  "\x1F6B6")
   where
      x = fromIntegral i
      y = fromIntegral j

drawLevelMap :: (Coords -> Tile) -> Picture
drawLevelMap levelmap = drawFromTo (-10, 11) (\j -> drawRow levelmap (-10, 11) j)

openDoors :: DoorColor -> (Coords -> Tile) -> (Coords -> Tile)
openDoors color currentMap = newMap
  where
    newMap :: Coords -> Tile
    newMap (Coords i j)
      | currentMap (Coords i j) == Door color = Floor
      | otherwise = currentMap (Coords i j)


tryMove :: Dir -> Coords -> Coords
tryMove dir (Coords i j)
  | dir == UpMove && canMove (levelMap (Coords i (j + 1))) = (Coords i (j + 1))
  | dir == DownMove && canMove (levelMap (Coords i (j - 1))) = (Coords i (j - 1))
  | dir == LeftMove && canMove (levelMap (Coords (i - 1) j)) = (Coords (i - 1) j)
  | dir == RightMove && canMove (levelMap (Coords (i + 1) j)) = (Coords (i + 1) j)
  | otherwise = (Coords i j)

canMove :: Tile -> Bool
canMove tile
  | tile == Floor = True
  | tile == Button = True
  | tile == Exit = True
  | otherwise = False

initialWorld :: Coords
initialWorld = Coords 0 0

handleWorld :: Event -> Coords -> Coords
handleWorld (KeyPress "Up") (Coords i j) = tryMove UpMove (Coords i j)
handleWorld (KeyPress "Down") (Coords i j) = tryMove DownMove (Coords i j)
handleWorld (KeyPress "Left") (Coords i j) = tryMove LeftMove (Coords i j)
handleWorld (KeyPress "Right") (Coords i j) = tryMove RightMove (Coords i j)
handleWorld _anyEvent coords = coords

renderWorld :: Coords -> Picture
renderWorld coords = scaled 0.8 0.8 (drawPlayer coords <> drawLevelMap (openDoors Red levelMap))

main :: IO ()
main = solution1
  where
    solution1 = activityOf initialWorld handleWorld renderWorld
