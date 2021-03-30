{-# LANGUAGE OverloadedStrings #-}
import CodeWorld

-- | Draw a circle for traffic lights given color and Y-offset.
lightCircle :: Double -> Color -> Picture
lightCircle y c = translated 0 y (colored c (solidCircle 1))

-- | Frame for traffic lights.
border :: Picture
border = rectangle 2.5 7.5

-- | Green traffic light.
greenLight :: Picture
greenLight = lightCircle (-2.375) green

-- | Red traffic light.
redLight :: Picture
redLight = lightCircle 2.375 red

-- | Yellow traffic light.
yellowLight :: Picture
yellowLight = lightCircle 0 yellow

-- | Green inactive traffic light.
greenInactiveLight :: Picture
greenInactiveLight = lightCircle (-2.375) grey

-- | Yellow inactive traffic light.
yellowInactiveLight :: Picture
yellowInactiveLight = lightCircle 0 grey

-- | Top inactive traffic light.
redInactiveLight :: Picture
redInactiveLight = lightCircle 2.375 grey



-- | Simple traffic lights with four states.
--
-- * '0' — red inactive, yelow inactive, green active
-- * '1' — red inactive, yelow active, green inactive
-- * '2' — red active, yelow inactive, green inactive
-- * '3' — red active, yelow active, green inactive
trafficLights :: Int -> Picture
trafficLights 0 = redInactiveLight <> yellowInactiveLight <> greenLight <> border
trafficLights 1 = redInactiveLight <> yellowLight <> greenInactiveLight <> border
trafficLights 2 = redLight <> yellowInactiveLight <> greenInactiveLight <> border
trafficLights 3 = redLight <> yellowLight <> greenInactiveLight <> border

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
