{-# LANGUAGE OverloadedStrings #-}
import CodeWorld

-- | Draw a circle for traffic lights given color and Y-offset.
lightCircle :: Double -> Double -> Color -> Picture
lightCircle x y c = translated x y (colored c (solidCircle 1))

pedestriansIcon :: Double -> Double -> Color -> Picture
pedestriansIcon x y c = translated x y (colored c (lettering  "\x1F6B6"))

cyclistsIcon :: Double -> Double -> Color -> Picture
cyclistsIcon x y c = translated x y (colored c (lettering  "\x1F6B2"))

-- | Frame for traffic lights.
border :: Picture
border = translated 4 (-1.25) (rectangle 2.5 5) <> rectangle 2.5 7.5 <> translated (-4) (-1.25) (rectangle 2.5 5)

-- | Green traffic light.
greenLight :: Picture
greenLight = lightCircle 0 (-2.375) green

-- | Red traffic light.
redLight :: Picture
redLight = lightCircle 0 2.375 red

-- | Yellow traffic light.
yellowLight :: Picture
yellowLight = lightCircle 0 0 yellow

-- | Green inactive traffic light.
greenInactiveLight :: Picture
greenInactiveLight = lightCircle 0 (-2.375) grey

-- | Yellow inactive traffic light.
yellowInactiveLight :: Picture
yellowInactiveLight = lightCircle 0 0 grey

-- | Top inactive traffic light.
redInactiveLight :: Picture
redInactiveLight = lightCircle 0 2.375 grey

-- | Green traffic light for pedestrians.
greenLightPedestrians :: Picture
greenLightPedestrians = lightCircle 4 (-2.375) green

-- | Red traffic light for pedestrians.
redLightPedestrians :: Picture
redLightPedestrians = lightCircle 4 0 red

-- | Green inactive traffic light for pedestrians.
greenInactiveLightPedestrians :: Picture
greenInactiveLightPedestrians = lightCircle 4 (-2.375) grey

-- | Top inactive traffic light for pedestrians.
redInactiveLightPedestrians :: Picture
redInactiveLightPedestrians = lightCircle 4 0 grey

-- | Icon for pedestrians at green light
pedestriansIconGreen :: Picture
pedestriansIconGreen = pedestriansIcon 4 (-2.375) black

-- | Icon for pedestrians at red light
pedestriansIconRed :: Picture
pedestriansIconRed = pedestriansIcon 4 0 black

-- | Green traffic light for cyclists.
greenLightCyclists :: Picture
greenLightCyclists = lightCircle (-4) (-2.375) green

-- | Red traffic light for cyclists.
redLightCyclists :: Picture
redLightCyclists = lightCircle (-4) 0 red

-- | Green inactive traffic light for cyclists.
greenInactiveLightCyclists :: Picture
greenInactiveLightCyclists = lightCircle (-4) (-2.375) grey

-- | Top inactive traffic light for cyclists.
redInactiveLightCyclists :: Picture
redInactiveLightCyclists = lightCircle (-4) 0 grey

-- | Icon for cyclists at green light
cyclistsIconGreen :: Picture
cyclistsIconGreen = cyclistsIcon (-4) (-2.375) black

-- | Icon for cyclists at red light
cyclistsIconRed :: Picture
cyclistsIconRed = cyclistsIcon (-4) 0 black

-- | Simple traffic lights with four states.
--
-- * '0' — red inactive, yelow inactive, green active
-- * '1' — red inactive, yelow active, green inactive
-- * '2' — red active, yelow inactive, green inactive
-- * '3' — red active, yelow active, green inactive
trafficLights :: Int -> Picture
trafficLights 0 = redInactiveLight <> yellowInactiveLight <> greenLight <> pedestriansIconGreen <> greenLightPedestrians <> redInactiveLightPedestrians  <> cyclistsIconGreen <> greenLightCyclists <> redInactiveLightCyclists <> border
trafficLights 1 = redInactiveLight <> yellowLight <> greenInactiveLight <> border
trafficLights 2 = redLight <> yellowInactiveLight <> greenInactiveLight <> pedestriansIconRed <> greenInactiveLightPedestrians <> redLightPedestrians <> cyclistsIconRed <> greenInactiveLightCyclists <> redLightCyclists <> border
trafficLights 3 = redLight <> yellowLight <> greenInactiveLight <> pedestriansIconRed <> greenInactiveLightPedestrians <> redLightPedestrians  <> cyclistsIconRed <> greenInactiveLightCyclists <> redLightCyclists <> border

-- | Traffic lights controller switching lights every 2 seconds.
trafficController :: Double  -> Picture
trafficController t
  | floor(t') `mod` 8 == 0 = trafficLights 3
  | floor(t') `mod` 4 == 0 = trafficLights 1
  | floor(t') `mod` 8 < 4 = trafficLights 0
  | otherwise = trafficLights 2
  where
    t' = t + 1.0

main :: IO()
main = animationOf trafficController
