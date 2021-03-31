{-# LANGUAGE OverloadedStrings #-}
import CodeWorld

lightCircle :: Double -> Double -> Color -> Picture
lightCircle x y c = translated x y (colored c (solidCircle 1))

lightsCars :: [Color] -> Picture
lightsCars [top, mid, bottom] = lightCircle 0 2.375 top <> lightCircle 0 0 mid <> lightCircle 0 (-2.375) bottom

lightsPedestrians:: [Color] -> Picture
lightsPedestrians [top, bottom] = lightCircle 4 0 top <>  lightCircle 4 (-2.375) bottom

lightsCyclists:: [Color] -> Picture
lightsCyclists [top, bottom] = lightCircle (-4) 0 top <>  lightCircle (-4) (-2.375) bottom

icons :: Picture
icons = translated 4 (-2.375) (lettering  "\x1F6B2") <> translated (-4) (-2.375) (lettering  "\x1F6B6")

-- | Frame for traffic lights.
borders :: Picture
borders = translated 4 (-1.25) (rectangle 2.5 5) <> rectangle 2.5 7.5 <> translated (-4) (-1.25) (rectangle 2.5 5)

stateOneCars = lightsCars [darker 0.33 red, darker 0.33 yellow, green]
stateTwoCars = lightsCars [darker 0.33 red, yellow, darker 0.33 green]
stateThreeCars = lightsCars [red, darker 0.33 yellow, darker 0.33 green]
stateFourCars = lightsCars [red, yellow, darker 0.33 green]

stateOnePedestriansAndCyclists = icons <> lightsPedestrians [darker 0.33 red, green] <> lightsCyclists[darker 0.33 red, green]
stateTwoPedestriansAndCyclists = lightsPedestrians [darker 0.33 red, darker 0.33 green] <> lightsCyclists[darker 0.33 red, darker 0.33 green]
stateThreePedestriansAndCyclists = lightsPedestrians [red, darker 0.33 green] <> lightsCyclists[red, darker 0.33 green]


-- | Simple traffic lights with four states.
--
-- * '0' — red inactive, yelow inactive, green active
-- * '1' — red inactive, yelow active, green inactive
-- * '2' — red active, yelow inactive, green inactive
-- * '3' — red active, yelow active, green inactive
trafficLights :: Int -> Picture
trafficLights 0 = stateOneCars <> stateOnePedestriansAndCyclists <> borders
trafficLights 1 = stateTwoCars <> stateTwoPedestriansAndCyclists <> borders
trafficLights 2 = stateTwoCars <> stateOnePedestriansAndCyclists <> borders
trafficLights 3 = stateThreeCars <> stateThreePedestriansAndCyclists <> borders
trafficLights 4 = stateFourCars <> stateThreePedestriansAndCyclists <> borders

-- | Traffic lights controller switching lights every 2 seconds.
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
