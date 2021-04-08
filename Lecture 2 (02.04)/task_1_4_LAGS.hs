{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHS -Wall #-}

import CodeWorld

data Tile = Wall | Floor | Door DoorColor  | Exit  | Button DoorColor deriving (Show, Eq)
data DoorColor = Red | Blue | Green deriving (Show, Eq)
data Coords = Coords Int Int
data Dir = LeftMove | RightMove | UpMove | DownMove deriving (Show, Eq)

doorColor :: DoorColor -> Color
doorColor Red = red
doorColor Blue = blue
doorColor Green = green

levelMap :: Coords -> Tile
levelMap (Coords i j)
  | (i, j) == (4, 4) = Door Red
  | (i, j) == (2, 4) = Button Red
  | (i, j) == (4, 1) = Door Green
  | (i, j) == (2, 1) = Button Green
  | i > 10 = Wall
  | i < -9 = Wall
  | j > 10 = Wall
  | j < -9 = Wall
  | otherwise = Floor



drawTile :: Tile -> Picture
drawTile Wall = colored black (solidRectangle 0.95 0.95)
drawTile Floor = colored yellow (solidRectangle 0.95 0.95)
drawTile (Button dc) = colored (doorColor dc) (solidCircle 0.45) <> colored yellow (solidRectangle 0.95 0.95)
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

canMove :: Tile -> Bool
canMove Floor = True
canMove (Button dc) = True
canMove Exit = True
canMove _any = False

tryMove :: (Coords -> Tile) -> Dir -> Coords -> Coords
tryMove currentMap dir (Coords i j)
  | dir == UpMove && canMove (currentMap (Coords i (j + 1))) = (Coords i (j + 1))
  | dir == DownMove && canMove (currentMap (Coords i (j - 1))) = (Coords i (j - 1))
  | dir == LeftMove && canMove (currentMap (Coords (i - 1) j)) = (Coords (i - 1) j)
  | dir == RightMove && canMove (currentMap (Coords (i + 1) j)) = (Coords (i + 1) j)
  | otherwise = (Coords i j)

openDoors :: [DoorColor] -> (Coords -> Tile) -> (Coords -> Tile)
openDoors colors currentMap = newMap
  where
    newMap :: Coords -> Tile
    newMap (Coords i j)
      | currentMap (Coords i j) == (Door dc) && dc `elem` colors = Floor
      | otherwise = currentMap (Coords i j)
      where
        (Door dc) = currentMap (Coords i j)

initialWorld :: (Coords, (Coords -> Tile))
initialWorld = ((Coords 0 0), levelMap)

handleWorld :: Event -> (Coords, (Coords -> Tile)) -> (Coords, (Coords -> Tile))
handleWorld (KeyPress "Up") ((Coords i j), currentMap) = (tryMove currentMap UpMove (Coords i j), currentMap)
handleWorld (KeyPress "Down") ((Coords i j), currentMap) = (tryMove currentMap DownMove (Coords i j), currentMap)
handleWorld (KeyPress "Left") ((Coords i j), currentMap) = (tryMove currentMap LeftMove (Coords i j), currentMap)
handleWorld (KeyPress "Right") ((Coords i j), currentMap) = (tryMove currentMap RightMove (Coords i j), currentMap)
handleWorld _anyEvent (coords, currentMap)
  | currentMap coords == (Button dc) = (coords, openDoors [dc] currentMap)
  | otherwise = (coords, currentMap)
  where
    (Button dc) = currentMap coords

renderWorld :: (Coords, (Coords -> Tile)) -> Picture
renderWorld (coords, currentMap) = scaled 0.8 0.8 (drawPlayer coords <> drawLevelMap currentMap)

main :: IO ()
main = solution1
  where
    solution1 = activityOf initialWorld handleWorld renderWorld
