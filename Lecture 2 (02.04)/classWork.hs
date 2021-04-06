import CodeWorld

data Tile = Wall | Floor | Door DoorColor  | Exit
data DoorColor = Red | Blue | Green
data Coords = Coords Int Int

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
  | otherwise = Door Red

drawFromTo :: (Int, Int) -> (Int -> Picture) -> Picture
drawFromTo (from, to) something
  | from > to = blank
  | otherwise = something from <> drawFromTo(from + 1, to) something

drawRow :: (Int, Int) -> Int -> Picture
drawRow (from, to) j = drawFromTo (from, to) (\i -> drawTileAt (Coords i j) (levelMap (Coords i j)))

drawRows :: (Int, Int) -> (Int, Int) -> Picture
drawRows (fromX, toX) (fromY, toY)
  = drawFromTo (fromY, toY) (\j -> drawRow (fromX, toX) j)

drawTileMap :: Picture
drawTileMap = drawRows (-6, 6) (-6, 6)

main :: IO ()
main = drawingOf drawTileMap
