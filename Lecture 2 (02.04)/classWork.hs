import CodeWorld

type Tile = Int
type Coords = (Int, Int)

floorTile :: Tile
floorTile = 0

wallTile :: Tile
wallTile = 1

drawTile :: Tile -> Picture
drawTile tile
  | tile == floorTile = colored yellow (solidRectangle 0.95 0.95)
  | tile == wallTile = colored black (solidRectangle 0.95 0.95)

drawTileAt :: Coords -> Tile -> Picture
drawTileAt (i, j) tile =
  translated x y (drawTile tile)
  where
    x = fromIntegral i
    y = fromIntegral j

tileMap :: Coords -> Tile
tileMap (i, j)
  | abs i > 5 = wallTile
  | abs j > 5 = wallTile
  | (i, j) == (2, 3) = wallTile
  | otherwise = floorTile

drawFromTo :: (Int, Int) -> (Int -> Picture) -> Picture
drawFromTo (from, to) something
  | from > to = blank
  | otherwise = something from <> drawFromTo(from + 1, to) something

drawRow :: (Int, Int) -> Int -> Picture
drawRow (from, to) j = drawFromTo (from, to) (\i -> drawTileAt (i, j) (tileMap (i, j)))

drawRows :: (Int, Int) -> (Int, Int) -> Picture
drawRows (fromX, toX) (fromY, toY)
  = drawFromTo (fromY, toY) (\j -> drawRow (fromX, toX) j)

drawTileMap :: Picture
drawTileMap = drawRows (-6, 6) (-6, 6)

main :: IO ()
main = drawingOf drawTileMap
