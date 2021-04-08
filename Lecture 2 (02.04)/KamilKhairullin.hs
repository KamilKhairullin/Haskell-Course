{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHS -Wall #-}

import CodeWorld

-- | Holds all possible constructors of type Tile. Tile is building block of our map
data Tile = Wall | Floor | Door DoorColor  | Exit  | Button DoorColor deriving (Show, Eq)
-- | Door color is color of doors and buttons
data DoorColor = Red | Pink | Green | LightGreen | Black deriving (Show, Eq)
-- | Holds coordinates of some object in the map in 2D cartesian coordinate system
data Coords = Coords Int Int
-- | Defines direction of moving
data Dir = LeftMove | RightMove | UpMove | DownMove deriving (Show, Eq)
-- | Holds colors of doors which are already open.
type State = [DoorColor]

-- | Map of our level
levelMap :: Coords -> Tile
levelMap (Coords i j)
  | (i, j) == (-9, -9) = Button Red
  | (i, j) == (10, 10) = Button Green
  | (i, j) == (10, -9) = Button Pink
  | (i, j) == (-9, 9) = Button Black
  | (i, j) == (-9, 0) = Button LightGreen
  | exit = Exit
  | melonBlack = Door Black
  | melonRed1 = Door Red
  | melonRed2 = Door Red
  | melonRed3 = Door Red
  | melonGreen1 = Door Green
  | melonGreen2 = Door Green
  | melonGreen3 = Door Green
  | melonLightGreen1 = Door LightGreen
  | melonLightGreen2 = Door LightGreen
  | melonLightGreen3 = Door LightGreen
  | melonLightPink1 = Door Pink
  | melonLightPink2 = Door Pink
  | melonLightPink3 = Door Pink
  | isMapBorder = Wall
  | otherwise = Floor
  where
    melonRed1 = (i == 3 && j == 7) || (j == 6  && (i > 1 && i < 4)) || (j == 5  && (i > 0 && i < 5))
    melonRed2 = (j == 4  && (i > (-1) && i < 5)) || (j == 3  && (i > (-2) && i < 5)) || (j == 2  && (i > (-3) && i < 5))
    melonRed3 = (j == 1  && (i > (-4) && i < 4)) || (j == 0  && (i > (-5) && i < 4)) || (j == -1  && (i > (-3) && i < 2))
    melonGreen1 = (j == 8 && i == 5) || (j == 7 && i == 6) || ((j < 7 && j > 0) && i == 7)
    melonGreen2 = ((j < 1 && j > -2) && i == 6) || ((j < 0 && j > -3) && i == 5) || ((j < -1 && j > -4) && i == 4)
    melonGreen3 = (j == -3 && i == 3) || (j == -4 && (i < 3 && i > -4)) || (j == -3 && i == -4) || (j == -2 && i == -5)
    melonLightGreen1 = (j == 8 && i == 4) || (j == 7 && i == 5) || ((j < 7 && j > 0) && i == 6)
    melonLightGreen2 = ((j < 1 && j > -2) && i == 5) || ((j < 0 && j > -3) && i == 4) || ((j < -1 && j > -4) && i == 3)
    melonLightGreen3 = (j == -3 && (i < 3 && i > -4)) || (j == -2 && i == -4) || (j == -1 && i == -5)
    melonLightPink1 = (j == 6 && i == 4) || (j == 7 && i == 4) || ((j < 7 && j > 0) && i == 5)
    melonLightPink2 = ((j < 2 && j > -2) && i == 4) || (j == -1 && (i > 1 && i < 4))
    melonLightPink3 = (j == -2 && (i < 3 && i > -4)) || (j == -1 && i == -3) || (j == -1 && i == -4)
    melonBlack = (j == 5 && i == 3) || (j == 3 && i == 2) || (j == 1 && i == 1) || (j == 0 && i == -2)
    --isHorizontalWall = j == 1 || j == 4
    exit = (j == 1 && i == 1)
    isMapBorder = i > 10 || i < -9 || j > 10 || j < -9

-- | Maps DoorColor type to CodeWorld color
--
-- * 'Red' — red
-- * 'Blue' — blue
-- * 'Green' - green
doorColor :: DoorColor -> Color
doorColor Red = bright red
doorColor Pink = bright pink
doorColor Green = darker 0.21 green
doorColor Black = black
doorColor LightGreen = darker 0.03 green

-- | returns CodeWorld Picture for any Tile
drawTile :: Tile -> Picture
drawTile Wall = colored black (solidRectangle 0.95 0.95)
drawTile Floor = colored yellow (solidRectangle 0.95 0.95)
drawTile (Button dc) = colored (doorColor dc) (solidCircle 0.45) <> colored yellow (solidRectangle 0.95 0.95)
drawTile Exit = rotated   (-180.0) (lettering  "\x1f349") <> colored yellow (solidRectangle 0.95 0.95)
drawTile (Door dc) = colored (doorColor dc) (solidRectangle 0.97 0.97)

-- | returns CodeWorld Picture for any Tile with given coordinates
drawTileAt :: Coords -> Tile -> Picture
drawTileAt (Coords i j) tile =
  translated x y (drawTile tile)
  where
    x = fromIntegral i
    y = fromIntegral j

-- | returns CodeWorld Picture of row using any level map
--
-- *  (Coords -> Tile) is map, based on which the picture is built
-- *  (Int, Int) is X-axis coordinates from and to which row will be drawn
-- * Int is Y-axis coordinate which specifies at what height the row will be drawn
drawRow :: (Coords -> Tile) -> (Int, Int) -> Int -> Picture
drawRow mapToDraw (from, to) j = drawFromTo (from, to) (\i -> drawTileAt (Coords i j) (mapToDraw (Coords i j)))

-- | draws any function (Int -> Picture) at given range
--
-- * Returns CodeWorld Picture
drawFromTo :: (Int, Int) -> (Int -> Picture) -> Picture
drawFromTo (from, to) something
  | from > to = blank
  | otherwise = something from <> drawFromTo(from + 1, to) something

-- | returns CodeWorld Picture of player lettering at given cartesian coordinates
drawPlayer :: Coords -> Picture
drawPlayer (Coords i j) = translated x y  (lettering  "\x1F6B6")
   where
      x = fromIntegral i
      y = fromIntegral j

-- | Draw a given level map of size 21x21.
drawLevelMap :: (Coords -> Tile) -> Picture
drawLevelMap levelmap = drawFromTo (-10, 11) (\j -> drawRow levelmap (-10, 11) j)

-- | Defines which tiles we can step on and which we cannot.
--
-- * Floor — can move
-- * Button of any color — can move
-- * Exit - can move
-- * any other tile - can't move
canMove :: Tile -> Bool
canMove Floor = True
canMove (Button dc) = True
canMove Exit = True
canMove _any = False

-- | Try to move character in a given direction.
tryMove :: (Coords -> Tile) -> Dir -> Coords -> Coords
tryMove currentMap dir (Coords i j)
  | dir == UpMove && canMove upTile = up
  | dir == DownMove && canMove downTile = down
  | dir == LeftMove && canMove leftTile = left
  | dir == RightMove && canMove rightTile = right
  | otherwise = (Coords i j)
  where
    up = Coords i (j + 1)
    down = Coords i (j - 1)
    left = Coords (i - 1) j
    right = Coords (i + 1) j
    upTile = currentMap up
    downTile = currentMap down
    leftTile = currentMap left
    rightTile = currentMap right

-- | Returns map with opened doors of given color
--
-- * [DoorColor] - tuple of colors which need to be opened
-- * (Coords -> Tile) - current game map
openDoors :: [DoorColor] -> (Coords -> Tile) -> (Coords -> Tile)
openDoors colors currentMap = newMap
  where
    newMap :: Coords -> Tile
    newMap coords
      | currentMap coords == (Door dc) && dc `elem` colors = Floor
      | otherwise = currentMap coords
      where
        (Door dc) = currentMap coords

-- | Initial world. The initial state of the activity.
--
-- * State is empty tuple. This means that doors of all colors are closed
-- * Initial coordinates of player = (0, 0)
-- * initial map is levelMap, which was defined above.
initialWorld :: (State, Coords, (Coords -> Tile))
initialWorld = ([], (Coords (-6) (-6)), levelMap)

-- | The event handling function, which updates the state given an event.
handleWorld :: Event -> (State, Coords, (Coords -> Tile)) -> (State, Coords, (Coords -> Tile))

-- | Actions when pressing the "up" button
--
-- * If we step on the button, open the doors ans steps up
-- * otherwise just update coordinates of player
handleWorld (KeyPress "Up") (state, (Coords i j), currentMap)
  | upperTile == (Button dc) && not isOpened = (newState, coordinatesWithNewMap, newMap)
  | otherwise = (state, coordinatesWithCurrentMap, currentMap)
  where
    (Button dc) = upperTile
    upperTile = currentMap (Coords i (j + 1))
    isButton = upperTile == (Button dc)
    isOpened = dc `elem` state
    coordinatesWithCurrentMap = tryMove currentMap UpMove (Coords i j)
    newState = [dc] ++ state
    coordinatesWithNewMap = tryMove newMap UpMove (Coords i j)
    newMap = openDoors [dc] currentMap

-- | Actions when pressing the "down" button
--
-- * If we step on the button, open the doors and steps down
-- * otherwise just update coordinates of player
handleWorld (KeyPress "Down") (state, (Coords i j), currentMap)
  | bottomTile == (Button dc)  && not isOpened = (newState, coordinatesWithNewMap, newMap)
  | otherwise = (state, coordinatesWithCurrentMap, currentMap)
  where
    (Button dc) = bottomTile
    bottomTile = currentMap (Coords i (j - 1))
    isButton = bottomTile == (Button dc)
    isOpened = dc `elem` state
    coordinatesWithCurrentMap = tryMove currentMap DownMove (Coords i j)
    newState = [dc] ++ state
    coordinatesWithNewMap = tryMove newMap DownMove (Coords i j)
    newMap = openDoors [dc] currentMap

-- | Actions when pressing the "left" button
--
-- * If we step on the button, open the doors and steps left
-- * otherwise just update coordinates of player
handleWorld (KeyPress "Left") (state, (Coords i j), currentMap)
  | leftTile == (Button dc)  && not isOpened = (newState, coordinatesWithNewMap, newMap)
  | otherwise = (state, coordinatesWithCurrentMap, currentMap)
  where
    (Button dc) = leftTile
    leftTile = currentMap (Coords (i - 1) j)
    isButton = leftTile == (Button dc)
    isOpened = dc `elem` state
    coordinatesWithCurrentMap = tryMove currentMap LeftMove (Coords i j)
    newState = [dc] ++ state
    coordinatesWithNewMap = tryMove newMap LeftMove (Coords i j)
    newMap = openDoors [dc] currentMap

-- | Actions when pressing the "right" button
--
-- * If we step on the button, open the doors and steps right
-- * otherwise just update coordinates of player
handleWorld (KeyPress "Right") (state, (Coords i j), currentMap)
  | rightTile == (Button dc)  && not isOpened = (newState, coordinatesWithNewMap, newMap)
  | otherwise = (state, coordinatesWithCurrentMap, currentMap)
  where
    (Button dc) = rightTile
    rightTile = currentMap (Coords (i + 1) j)
    isButton = rightTile == (Button dc)
    isOpened = dc `elem` state
    coordinatesWithCurrentMap = tryMove currentMap RightMove (Coords i j)
    newState = [dc] ++ state
    coordinatesWithNewMap = tryMove newMap RightMove (Coords i j)
    newMap = openDoors [dc] currentMap

-- | Action handler for all other actions. Does nothing
handleWorld _anyEvent (state, (Coords i j), currentMap) = (state, (Coords i j), currentMap)

-- | The visualization function, which converts the state into a picture to display.
renderWorld :: (State, Coords, (Coords -> Tile)) -> Picture
renderWorld (state, coords, currentMap) = scaled scaleMultiplicator scaleMultiplicator (drawPlayer coords <> drawLevelMap currentMap)
  where
    scaleMultiplicator = 0.8

main :: IO ()
main = solution4
  where
    solution4 = activityOf initialWorld handleWorld renderWorld
