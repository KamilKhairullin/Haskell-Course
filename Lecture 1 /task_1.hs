{-# LANGUAGE OverloadedStrings #-}
import CodeWorld

-- | Draw a circle for traffic lights given color, X-offset, Y-offset.
lightCircle :: Double -> Double -> Color -> Picture
lightCircle x y c = translated x y (colored c (solidCircle 1))

-- | Draw lights for cars traffic light given colors for top, mid and bottom light
lightsCars :: [Color] -> Picture
lightsCars [top, mid, bottom] = lightCircle 0 2.375 top <> lightCircle 0 0 mid <> lightCircle 0 (-2.375) bottom

-- | Draw lights for pedestrians traffic light given colors for top, mid and bottom light
lightsPedestrians:: [Color] -> Picture
lightsPedestrians [top, bottom] = lightCircle 4 0 top <>  lightCircle 4 (-2.375) bottom

-- | Draw lights for cyclists traffic light given colors for top, mid and bottom light
lightsCyclists:: [Color] -> Picture
lightsCyclists [top, bottom] = lightCircle (-4) 0 top <>  lightCircle (-4) (-2.375) bottom

-- | Draw icons for cyclists and pedestrians traffic lights
icons :: Picture
icons = translated 4 (-2.375) (lettering  "\x1F6B2") <> translated (-4) (-2.375) (lettering  "\x1F6B6")

-- | Frame for traffic lights.
borders :: Picture
borders = translated 4 (-1.25) (rectangle 2.5 5) <> rectangle 2.5 7.5 <> translated (-4) (-1.25) (rectangle 2.5 5)

-- | First state for car traffic lights [RED OFF, YELLOW OFF, GREEN ON]
stateOneCars = lightsCars [darker 0.33 red, darker 0.33 yellow, green]

-- | Second state for car traffic lights [RED OFF, YELLOW ON, GREEN OFF]
stateTwoCars = lightsCars [darker 0.33 red, yellow, darker 0.33 green]

-- | Third state for car traffic lights [RED ON, YELLOW OFF, GREEN OFF]
stateThreeCars = lightsCars [red, darker 0.33 yellow, darker 0.33 green]

-- | Fourth state for car traffic lights [RED ON, YELLOW OFF, GREEN ON]
stateFourCars = lightsCars [red, yellow, darker 0.33 green]

-- | First state for car traffic lights [RED OFF, GREEN ON], ICONS ON
stateOnePedestriansAndCyclists = icons <> lightsPedestrians [darker 0.33 red, green] <> lightsCyclists[darker 0.33 red, green]

-- | Second state for car traffic lights [RED OFF, GREEN OFF], ICONS OFF
stateTwoPedestriansAndCyclists = lightsPedestrians [darker 0.33 red, darker 0.33 green] <> lightsCyclists[darker 0.33 red, darker 0.33 green]

-- | Third state for car traffic lights [RED ON, GREEN OFF], ICONS OFF
stateThreePedestriansAndCyclists = lightsPedestrians [red, darker 0.33 green] <> lightsCyclists[red, darker 0.33 green]


-- | Simple traffic lights with five states.
--
-- * '0' — red inactive, yelow inactive, green active
-- * '1' — red inactive, yelow active, green inactive
-- * '2' — red inactive, yelow active, green inactive
-- * '3' — red active, yelow inactive, green inactive
-- * '4' — red active, yelow active, green inactive
trafficLights :: Int -> Picture
trafficLights 0 = stateOneCars <> stateOnePedestriansAndCyclists <> borders
trafficLights 1 = stateTwoCars <> stateTwoPedestriansAndCyclists <> borders
trafficLights 2 = stateTwoCars <> stateOnePedestriansAndCyclists <> borders
trafficLights 3 = stateThreeCars <> stateThreePedestriansAndCyclists <> borders
trafficLights 4 = stateFourCars <> stateThreePedestriansAndCyclists <> borders

-- | Traffic lights controller switching lights.For cars 1st state - 3sec, 2nd state - 1 sec, 3rd state - 3 sec, 4th state - 1 sec
-- | For pedestrians and cyclists 1st state - 3sec, 2nd state - 1 sec, 3rd state - 4 sec
trafficController :: Double  -> Picture
trafficController t
  | floor(t') `mod` 8 == 0 = trafficLights 4
  | floor(t') `mod` 4 == 0 && (isOdd < 4 || (isOdd > 8 && isOdd < 12)) = trafficLights 1
  | floor(t') `mod` 4 == 0 = trafficLights 2
  | floor(t') `mod` 8 < 4 = trafficLights 0
  | otherwise = trafficLights 3
  where
    t' = t + 1.0
    isOdd = floor((t - fromIntegral (floor(t))) * 16)

main :: IO()
main = animationOf trafficController