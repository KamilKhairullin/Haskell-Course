{-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
import CodeWorld
import Prelude hiding (lookup) 
import Data.Map.Strict 
import Data.Foldable 
import Data.List

data Tile = Background | Picker | Pipe PipeType Double | Water | Empty | Finish deriving (Show, Eq, Ord) 
data Coords = Coords Int Int deriving (Show, Eq, Ord) 
data PipeType = Line | Angle | Cross deriving (Show, Eq, Ord) 


levelMap :: Map Coords Tile
levelMap = fromList (generateMap mapBorders mapBorders)

pipeRotation :: Map Int Tile
pipeRotation = fromList [(0, p1), (1, p2), (2, p3), (3, p4), (4, p5), (5, p6)]
  where
    p1 = Pipe Line 0
    p2 = Pipe Line 90
    p3 = Pipe Angle 0
    p4 = Pipe Angle 90
    p5 = Pipe Angle 180
    p6 = Pipe Angle 270
    
levelGraph :: Map (Coords, Tile) [(Coords, Tile)] 
levelGraph = fromList []

mapBorders :: Coords
mapBorders = Coords (-4) 5
 
isInBorder :: Coords -> Coords -> Bool
isInBorder borders coords = answer
  where 
    (Coords b1 b2) = borders
    (Coords x y) = coords
    answer = x < b2 && x >= b1 && y < b2 && y > b1
---------------------------------------------------------------------------------------------------------------------------- 
-- | Given a graph G, a node u, and an integer k,
--   find all nodes reachable from u in exactly
--   k steps in G. This may visit a node more than once.
breadthFirst :: Map (Coords, Tile) [(Coords, Tile)]  -> (Coords, Tile) -> Int -> [(Coords,  Tile)]  -> [(Coords,  Tile)]
breadthFirst _ node 0 visited = visited
breadthFirst g node dist visited = maybe [] (\l -> fold $ fmap a l) (Data.Map.Strict.lookup node g)
  where
    a = \p -> if
            | p `elem` newVisited -> newVisited
            | otherwise -> breadthFirst g p (dist-1) newVisited
    newVisited = visited ++ [node]
    allNear = Data.Map.Strict.lookup node g
      
----------------------------------------------------------------------------------------------------------------------------   
addLineToGraph :: Map (Coords, Tile) [(Coords, Tile)]  -> Map Coords Tile -> Coords -> Int -> Map (Coords, Tile) [(Coords, Tile)] 
addLineToGraph graph mymap coords pipeType
  | correctnessFirstNeighbor && correctnessSecondNeighbor = unionsWith (++) [graph, mapLeft, mapRight, mapCurrent]
  | correctnessFirstNeighbor = unions[graph, mapLeft, mapCurrentLeft]
  | correctnessSecondNeighbor = unions[graph, mapRight, mapCurrentRight]
  | otherwise = graph
  where
    (Coords i j) = coords
    firstNeighborCoords
      | pipeType == 0 = (Coords (i-1) j)
      | otherwise = (Coords i (j-1))
    secondNeighborCoords
      | pipeType == 0 = (Coords (i+1) j)
      | otherwise = (Coords i (j+1))
      
    firstNeighborTile 
      | isInBorder mapBorders firstNeighborCoords = mymap ! firstNeighborCoords
      | otherwise = Empty
     
    secondNeighborTile 
      | isInBorder mapBorders secondNeighborCoords = mymap ! secondNeighborCoords
      | otherwise = Empty
     
    currentTile = mymap ! coords 
    
    correctnessFirstNeighbor
      | pipeType == 0 = isCorrectLeft firstNeighborTile
      | otherwise = isCorrectBot firstNeighborTile
      
    correctnessSecondNeighbor
      | pipeType == 0 = isCorrectRight secondNeighborTile
      | otherwise = isCorrectTop secondNeighborTile
      
    mapLeft = fromList[((firstNeighborCoords, firstNeighborTile), [(coords, currentTile)])]
    mapRight = fromList[((secondNeighborCoords, secondNeighborTile), [(coords, currentTile)])]
    mapCurrent = fromListWith (++) [((coords, currentTile), [(firstNeighborCoords, firstNeighborTile)]), ((coords, currentTile), [(secondNeighborCoords, secondNeighborTile)])]
    mapCurrentLeft = fromList[((coords, currentTile), [(firstNeighborCoords, firstNeighborTile)])]
    mapCurrentRight = fromList[((coords, currentTile), [(secondNeighborCoords, secondNeighborTile)])]
  

addAngleToGraph :: Map (Coords, Tile) [(Coords, Tile)]  -> Map Coords Tile -> Coords -> Int -> Map (Coords, Tile) [(Coords, Tile)] 
addAngleToGraph graph mymap coords pipeType
  | correctnessFirstNeighbor && correctnessSecondNeighbor = unionsWith (++) [graph, mapLeft, mapRight, mapCurrent]
  | correctnessFirstNeighbor = unions[graph, mapLeft, mapCurrentLeft]
  | correctnessSecondNeighbor = unions[graph, mapRight, mapCurrentRight]
  | otherwise = graph
  where
    (Coords i j) = coords
    firstNeighborCoords
      | pipeType == 0 || pipeType == 3 = (Coords (i+1) j)
      | otherwise = (Coords (i-1) j)
    secondNeighborCoords
      | pipeType == 0 || pipeType == 1 = (Coords i (j+1))
      | otherwise = (Coords i (j-1))
      
    firstNeighborTile 
      | isInBorder mapBorders firstNeighborCoords = mymap ! firstNeighborCoords
      | otherwise = Empty
     
    secondNeighborTile 
      | isInBorder mapBorders secondNeighborCoords = mymap ! secondNeighborCoords
      | otherwise = Empty
     
    currentTile = mymap ! coords

    correctnessFirstNeighbor
      | pipeType == 0 || pipeType == 3 = isCorrectRight firstNeighborTile
      | otherwise = isCorrectLeft firstNeighborTile
      
    correctnessSecondNeighbor
      | pipeType == 0 || pipeType == 1 = isCorrectTop secondNeighborTile
      | otherwise = isCorrectBot secondNeighborTile
      
    
    mapLeft = fromList[((firstNeighborCoords, firstNeighborTile), [(coords, currentTile)])]
    mapRight = fromList[((secondNeighborCoords, secondNeighborTile), [(coords, currentTile)])]
    mapCurrent = fromListWith (++) [((coords, currentTile), [(firstNeighborCoords, firstNeighborTile)]), ((coords, currentTile), [(secondNeighborCoords, secondNeighborTile)])]
    mapCurrentLeft = fromList[((coords, currentTile), [(firstNeighborCoords, firstNeighborTile)])]
    mapCurrentRight = fromList[((coords, currentTile), [(secondNeighborCoords, secondNeighborTile)])]

isCorrectLeft :: Tile -> Bool
isCorrectLeft (Pipe Line 0) = True
isCorrectLeft (Pipe Angle 0) = True
isCorrectLeft (Pipe Angle 270) = True
isCorrectLeft (Pipe Cross 0) = True
isCorrectLeft _ = False

isCorrectRight :: Tile -> Bool
isCorrectRight (Pipe Line 0) = True
isCorrectRight (Pipe Angle 90) = True
isCorrectRight (Pipe Angle 180) = True
isCorrectRight (Pipe Cross 0) = True
isCorrectRight _ = False

isCorrectTop :: Tile -> Bool
isCorrectTop (Pipe Line 90) = True
isCorrectTop (Pipe Angle 270) = True
isCorrectTop (Pipe Angle 180) = True
isCorrectTop (Pipe Cross 0) = True
isCorrectTop _ = False

isCorrectBot :: Tile -> Bool
isCorrectBot (Pipe Line 90) = True
isCorrectBot (Pipe Angle 90) = True
isCorrectBot (Pipe Angle 0) = True
isCorrectBot (Pipe Cross 0) = True
isCorrectBot _ = False
----------------------------------------------------------------------------------------------------------------------------    

generateMap :: Coords -> Coords -> [(Coords, Tile)] 
generateMap (Coords yFrom yTo) (Coords xFrom xTo) 
  | yFrom <= yTo = (generateRow yFrom (Coords xFrom xTo)) ++ generateMap (Coords (yFrom + 1) yTo) (Coords xFrom xTo)
  | otherwise = []
    
generateRow :: Int -> Coords -> [(Coords, Tile)] 
generateRow y (Coords from to)
  | y == 4 && from == 4   = [(currentCoords, (Pipe Line 0))] ++ (generateRow y newCoords)
  | y == (-4) && from == (0)   = [(currentCoords, (Finish))] ++ (generateRow y newCoords)
  | from <= to = [(currentCoords, b)] ++ (generateRow y newCoords)
  | otherwise = []
  where
    newCoords = (Coords (from + 1) to)
    currentCoords = Coords from y
    b = Background
    

----------------------------------------------------------------------------------------------------------------------------
updateGraph :: Coords -> Coords -> Map (Coords, Tile) [(Coords, Tile)]  -> Map Coords Tile ->  Map (Coords, Tile) [(Coords, Tile)]
updateGraph (Coords yFrom yTo) (Coords xFrom xTo) graph mymap
  | yFrom <= yTo = updateGraph  (Coords (yFrom+1) yTo) (Coords xFrom xTo) newGraph mymap
  | otherwise = graph
    where 
      newGraph = updateRow yFrom (Coords xFrom xTo) graph mymap
  
updateRow :: Int -> Coords -> Map (Coords, Tile) [(Coords, Tile)]  -> Map Coords Tile ->  Map (Coords, Tile) [(Coords, Tile)]
updateRow y (Coords xFrom xTo) graph mymap
  | xFrom < xTo = updateRow y (Coords (xFrom+1) xTo) newGraph mymap 
  | otherwise = graph
  where
    currentTile = mymap ! (Coords xFrom y)
    newGraph 
      | currentTile == (Pipe Line 0) = addLineToGraph graph mymap (Coords xFrom y) 0
      | currentTile == (Pipe Line 90) = addLineToGraph graph mymap (Coords xFrom y) 1
      | currentTile == (Pipe Angle 0) = addAngleToGraph graph mymap (Coords xFrom y) 0
      | currentTile == (Pipe Angle 90) = addAngleToGraph graph mymap (Coords xFrom y) 1
      | currentTile == (Pipe Angle 180) = addAngleToGraph graph mymap (Coords xFrom y) 2
      | otherwise = graph
    


----------------------------------------------------------------------------------------------------------------------------
-- | returns CodeWorld Picture for any Tile with given coordinates
drawTileAt :: Coords -> Maybe Tile -> Picture
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
drawRow :: (Map Coords Tile) -> Coords -> Int -> Picture
drawRow myMap coords j = drawFromTo coords (\i -> drawTileAt (Coords i j) (Data.Map.Strict.lookup (Coords i j) myMap))


drawFromTo :: Coords -> (Int -> Picture) -> Picture
drawFromTo coords something
  | from >= to = blank
  | otherwise = something from <> drawFromTo(Coords (from + 1) to) something
  where
    Coords from to = coords
    
drawLevelMap :: Map Coords Tile -> Picture
drawLevelMap mymap = drawFromTo mapBorders (\j -> drawRow mymap mapBorders j)


drawWaterPoints :: [(Coords,  Tile)] -> Picture
drawWaterPoints [] = blank
drawWaterPoints ((coords, _) : xs) = (drawTileAt coords (Just Water)) <> drawWaterPoints xs 
    
drawNextPipes :: Int -> Picture
drawNextPipes currentBlockNumber = elements 
  where
    elements = picture1 <>  picture2 <> picture3
    currentPipe = pipeRotation ! i1
    i1 = currentBlockNumber `mod` 6
    nextPipe = pipeRotation ! i2
    i2 = (currentBlockNumber+1) `mod` 6
    thirdPipe = pipeRotation ! i3
    i3 = (currentBlockNumber+2) `mod` 6
    picture1 = drawTileAt (Coords (-2) (-7)) (Just currentPipe) <> translated (-2) (-7) background_next
    picture2 = drawTileAt (Coords (0) (-7)) (Just nextPipe) <> translated 0 (-7) background
    picture3 = drawTileAt (Coords 2 (-7)) (Just thirdPipe) <>  translated 2 (-7) background
    background = colored white (solidRectangle 1 1) <> colored black (solidRectangle 1.2 1.2)
    background_next = colored white (solidRectangle 1 1) <> colored red (solidRectangle 1.2 1.2)
    
drawTile :: Maybe Tile -> Picture
drawTile (Just Background) = colored white (solidRectangle 0.9 0.9) <> colored black (solidRectangle 1 1)
drawTile (Just Water) =  colored blue (solidRectangle 0.25 0.25)
drawTile (Just Picker) = colored red (thickRectangle 0.15 1.0 1.0)
drawTile (Just (Pipe Line rotation)) = rotated (pi / 180 * rotation) linePipe
  where 
    linePipe = (translated 0.25 0 (colored green (solidRectangle 0.51 0.5)) <> translated (-0.25) 0 (colored green (solidRectangle 0.51 0.5)))
drawTile (Just (Pipe Angle rotation)) = rotated (pi / 180 * rotation) anglePipe
  where
    anglePipe = (translated 0.25 0 (colored green (solidRectangle 0.51 0.5)) <> translated 0 (0.12)  (colored green (solidRectangle 0.51 0.75)))    
drawTile (Just(Pipe Cross rotation)) = rotated (pi / 180 * rotation) crossPipe
  where
    crossPipe = colored green (solidRectangle 0.5 1) <> colored green (solidRectangle 1 0.5)
drawTile (Just(Finish)) = colored black (solidCircle 0.45)
drawTile _ = blank
----------------------------------------------------------------------------------------------------------------------------

initialWorld :: (Int, Coords, (Map Coords Tile), Map (Coords, Tile) [(Coords, Tile)] )
initialWorld = (1, (Coords (4) 4), levelMap, levelGraph)


handleWorld :: Event -> (Int, Coords, (Map Coords Tile), Map (Coords, Tile) [(Coords, Tile)] ) -> (Int, Coords, (Map Coords Tile), Map (Coords, Tile) [(Coords, Tile)] )

handleWorld (KeyPress "Up") (blockNumber, pickerCoords, currentMap, graph)
  | isMovable = (blockNumber, (Coords i (j+1)), currentMap, graph)
  | otherwise = (blockNumber, pickerCoords, currentMap, graph)
  where
    Coords i j = pickerCoords
    Coords _ mapJ = mapBorders
    isMovable = j < mapJ - 1

handleWorld (KeyPress "Down") (blockNumber, pickerCoords, currentMap, graph)
  | isMovable = (blockNumber, (Coords i (j-1)), currentMap, graph)
  | otherwise = (blockNumber, pickerCoords, currentMap, graph)
  where
    Coords i j = pickerCoords
    Coords mapI _ = mapBorders
    isMovable = j > mapI
    
handleWorld (KeyPress "Left") (blockNumber, pickerCoords, currentMap, graph)
  | isMovable = (blockNumber, (Coords (i-1) j), currentMap, graph)
  | otherwise = (blockNumber, pickerCoords, currentMap, graph)
  where
    Coords i j = pickerCoords
    Coords mapI _ = mapBorders
    isMovable = i > mapI
        
handleWorld (KeyPress "Right") (blockNumber, pickerCoords, currentMap, graph)
  | isMovable = (blockNumber, (Coords (i+1) j), currentMap, graph)
  | otherwise = (blockNumber, pickerCoords, currentMap, graph)
  where
    Coords i j = pickerCoords
    Coords _ mapJ = mapBorders
    isMovable = i < mapJ - 1
   
handleWorld (KeyPress "Enter") (blockNumber, pickerCoords, currentMap, graph) = ((blockNumber + 1), pickerCoords, newMap, newGraph) 
  where
    newMap = Data.Map.Strict.insert pickerCoords newTile currentMap
    newTile = pipeRotation ! i 
    i = blockNumber `mod` 6
    newGraph 
      | newTile == (Pipe Line 0) = addLineToGraph graph currentMap pickerCoords 0
      | newTile == (Pipe Line 90) = addLineToGraph graph currentMap pickerCoords 1
      | newTile == (Pipe Angle 0) = addAngleToGraph graph currentMap pickerCoords 0
      | newTile == (Pipe Angle 90) = addAngleToGraph graph currentMap pickerCoords 1
      | newTile == (Pipe Angle 180) = addAngleToGraph graph currentMap pickerCoords 2
      | otherwise = addAngleToGraph graph currentMap pickerCoords 3
    --gameOver 
    --  | left = True
   --left = (currentMap ! (Coords (-1) (-4))) == (Pipe Line 0) || (Pipe Angle 270) || (Pipe Angle 0)
     
handleWorld _anyEvent (blockNumber, pickerCoords, currentMap, graph) = (blockNumber, pickerCoords, currentMap, graph)

renderWorld :: (Int, Coords, (Map Coords Tile), Map (Coords, Tile) [(Coords, Tile)]) -> Picture
renderWorld (blockNumber, pickerCoords, currentMap, graph) = scaled scaleMultiplicator scaleMultiplicator (drawNextPipes blockNumber <> drawWaterPoints points <> drawTileAt pickerCoords (Just Picker) <> drawLevelMap currentMap)
  where
    updGraph = updateGraph mapBorders mapBorders graph currentMap
    scaleMultiplicator = 1
    points = nub(breadthFirst updGraph searchStart 40 [])
    searchStart = ((Coords (4) 4), (Pipe Line 0)) 
 ----------------------------------------------------------------------------------------------------------------------------   
  

main :: IO ()
main = activityOf initialWorld handleWorld renderWorld

