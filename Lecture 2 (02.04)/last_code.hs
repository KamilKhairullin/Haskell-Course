{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHS -Wall #-}

import CodeWorld

data Tile = Wall | Floor | Door DoorColor  | Exit  | Button DoorColor deriving (Show, Eq)
data DoorColor = Red | Blue | Green deriving (Show, Eq)
data Coords = Coords Int Int
data Dir = LeftMove | RightMove | UpMove | DownMove deriving (Show, Eq)

levelMap :: Coords -> Tile
levelMap (Coords i j)
  | (i, j) == (4, 4) = Door Red
--  | (i, j) == (1, 1) = Door Blue
--  | j == 3 = Wall
  | i > 10 = Wall
  | i < -9 = Wall
  | j > 10 = Wall
  | j < -9 = Wall
--  | (i, j) == (2, 3) = Wall
  | (i, j) == (2, 4) = Button Red
--  | (i, j) == (3, 2) = Exit
  | otherwise = Floor

openDoors :: [DoorColor] -> (Coords -> Tile) -> (Coords -> Tile)
openDoors colors currentMap = newMap
  where
    newMap :: Coords -> Tile
    newMap (Coords i j)
      | currentMap (Coords i j) == (Door dc) && dc `elem` colors = Floor
      | otherwise = currentMap (Coords i j)
      where
        (Door dc) = currentMap (Coords i j)

compareTileAt :: (Int, Int) -> (Coords -> Tile) -> (Coords -> Tile) -> Bool
compareTileAt (a, b) map1 map2
  | map1 (Coords a b) == map2 (Coords a b) = True
  | otherwise = False

compareRows :: (Int, Int) -> Int -> (Coords -> Tile) -> (Coords -> Tile) -> Bool
compareRows (from, to) j map1 map2
  | from > to = True
  | isSame = compareRows (from + 1, to) j map1 map2
  | otherwise = False
  where
    isSame = compareTileAt (from, j) map1 map2

compareMaps :: (Int, Int) -> (Coords -> Tile) -> (Coords -> Tile) -> Bool
compareMaps (from, to) map1 map2
  | from > to = True
  | isSame = compareMaps(from + 1, to) map1 map2
  | otherwise = False
  where
    isSame = compareRows ((-10), 11) from map1 map2

getMap :: (Int, Int) -> (Coords -> Tile) -> (Coords -> Tile)
getMap (i, j) currentMap
  | areMapsSame levelMap && isButton = openDoors [dc] currentMap
  | areMapsSame (openDoors [Red] levelMap)  && isButton = openDoors [Red, dc] currentMap
  | areMapsSame (openDoors [Green] levelMap) && isButton = openDoors [Green, dc] currentMap
  | areMapsSame (openDoors [Blue] levelMap) && isButton = openDoors [Blue, dc] currentMap
  | areMapsSame (openDoors [Red, Green] levelMap) && isButton = openDoors [Red, Green, dc] currentMap
  | areMapsSame (openDoors [Red, Blue] levelMap) && isButton = openDoors [Red, Blue, dc] currentMap
  | areMapsSame (openDoors [Blue, Green] levelMap) && isButton = openDoors [Blue, Green, dc] currentMap
  | otherwise = (openDoors [Red, Green, Blue] levelMap)
  where
    areMapsSame :: (Coords -> Tile) -> Bool
    areMapsSame someMap = compareMaps (-10, 11) someMap currentMap
    isButton = currentTile == (Button dc)
    currentTile = levelMap (Coords i j)
    (Button dc) = currentMap (Coords i j)

doorColor :: DoorColor -> Color
doorColor Red = red
doorColor Blue = blue
doorColor Green = green

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


tryMove :: Dir -> Coords -> Coords
tryMove dir (Coords i j)
  | dir == UpMove && canMove (currentMap (Coords i (j - 1))) = (Coords i (j + 1))
  | dir == DownMove && canMove (currentMap (Coords i (j - 1))) = (Coords i (j - 1))
  | dir == LeftMove && canMove (currentMap (Coords (i - 1) j)) = (Coords (i - 1) j)
  | dir == RightMove && canMove (currentMap (Coords (i + 1) j)) = (Coords (i + 1) j)
  | otherwise = (Coords i j)
  where
    currentMap =  getMap (i , j) levelMap


canMove :: Tile -> Bool
canMove Floor = True
canMove (Button dc) = True
canMove Exit = True
canMove _any = False

initialWorld :: Coords
initialWorld = Coords 0 0

handleWorld :: Event -> Coords -> Coords
handleWorld (KeyPress "Up") (Coords i j) = tryMove UpMove (Coords i j)
handleWorld (KeyPress "Down") (Coords i j) = tryMove DownMove (Coords i j)
handleWorld (KeyPress "Left") (Coords i j) = tryMove LeftMove (Coords i j)
handleWorld (KeyPress "Right") (Coords i j) = tryMove RightMove (Coords i j)
handleWorld _anyEvent coords = coords

renderWorld :: (Coords -> Tile) -> Coords -> Picture
renderWorld myMap (Coords i j)
  | compareMaps (-10, 11) myMap (getMap (i, j) myMap) = scaled 0.8 0.8 (drawPlayer (Coords i j)) <> drawLevelMap (myMap)
  | otherwise = renderWorld (getMap (i, j) myMap) (Coords i j)


main :: IO ()
main = solution1
  where
    solution1 = activityOf initialWorld handleWorld (renderWorld levelMap)
