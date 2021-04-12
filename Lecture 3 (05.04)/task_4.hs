{-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}
{-# LANGUAGE OverloadedStrings #-}
import CodeWorld

-- | Holds all possible values of type Button
data Button = Up | Down | Stop deriving (Eq, Show)

-- | Holds all possible values of type Mode, which is state of moving of lift
data Mode = Idle | MovingUp | MovingDown deriving (Eq, Show)

-- | Draws circle
templateCircle :: Picture
templateCircle = colored (light grey) (solidCircle 0.425) <> colored black (solidCircle 0.5)

templateRectangle :: Picture
templateRectangle = colored (light grey) (solidRectangle 1.45 2.95) <> colored black (solidRectangle 1.5 3)

topArrow :: Picture
topArrow = translated 0 0.7 (scaled 1.2 1.2 (lettering  "\x1F809"))

bottomArrow :: Picture
bottomArrow = translated 0 (-0.9) (scaled 1.2 1.2 (lettering  "\x1F80B"))

drawMode :: Mode -> Picture
drawMode Idle = colored white topArrow <> colored white bottomArrow <> templateRectangle
drawMode MovingUp = colored red topArrow <> colored white bottomArrow <> templateRectangle
drawMode MovingDown = colored white topArrow <> colored red bottomArrow <> templateRectangle


drawButton :: Button -> Picture
drawButton Up = (scaled 0.8 0.8 (colored white (lettering  "\x1F809"))) <> templateCircle
drawButton Down = (scaled 0.8 0.8 (colored white (lettering  "\x1F80B"))) <> templateCircle
drawButton Stop = (scaled 0.3 0.3 (colored red (lettering  "STOP"))) <> templateCircle

asSpaced :: Double -> (a -> Picture) -> [a] -> Picture
asSpaced _ _ [] = blank
asSpaced range f (x:xs) = f x <> shifted (asSpaced range f xs)
  where
    shifted = translated range 0

elevator :: Mode -> [(Button, Mode)]
elevator Idle = [(Up, MovingUp), (Down, MovingDown)]
elevator MovingUp = [(Stop, Idle)]
elevator MovingDown = [(Stop, Idle)]

applyAction :: Maybe a -> (a -> a -> Bool) -> (s -> [(a, s)]) -> s -> s
applyAction (Just key) equalityTest mapping currentValue = newValue
  where
    newValue = findValueByKey currentValue (mapping currentValue) equalityTest key

applyAction Nothing _ _ currentValue = currentValue


findValueByKey :: y -> [(x, y)] -> (x -> x -> Bool) -> x -> y
findValueByKey currentMode [] _ _ = currentMode
findValueByKey currentMode ((x, y) : xs) equalCheck key
  | equalCheck x key = y
  | otherwise = findValueByKey currentMode xs equalCheck key

returnAllValues :: y -> [(x, y)] -> (x -> x -> Bool) -> [x]
returnAllValues _ [] _  = []
returnAllValues currentMode ((x, _) : xs) equalCheck
    = [x] ++ (returnAllValues currentMode xs equalCheck)


isEqual :: Button -> Button -> Bool
isEqual x y
  | x == y = True
  | otherwise = False

initialWorld :: (Mode, Double, Double)
initialWorld = (Idle, 0.0, 0.0)

handleWorld :: Event -> (Mode, Double, Double) -> (Mode, Double, Double)
handleWorld (KeyPress "Up") (currentMode, coordinates, lastStop) = (applyAction (Just Up) isEqual elevator currentMode, coordinates, lastStop)
handleWorld (KeyPress "Down") (currentMode, coordinates, lastStop) = (applyAction (Just Down) isEqual elevator currentMode, coordinates, lastStop)
handleWorld (KeyPress " ") (currentMode, coordinates, _) = (applyAction (Just Stop) isEqual elevator currentMode, coordinates, coordinates)
handleWorld (TimePassing dt) (Idle, coordinates, lastStop) = (Idle, lastStop, lastStop)
handleWorld (TimePassing dt) (MovingUp, coordinates, lastStop) = (MovingUp, coordinates + dt, lastStop)
handleWorld (TimePassing dt) (MovingDown, coordinates, lastStop) = (MovingDown, coordinates - dt, lastStop)
handleWorld _anyEvent (currentMode, time, lastStop) = (currentMode, time, lastStop)

interactiveFSM :: (s, Double, Double) -- ˆ Initial state.
  -> (a -> a -> Bool) -- ˆ Action equality test.
  -> (s -> [(a, s)]) -- ˆ State transitions.
  -> (Event -> (s, Double, Double) -> (s, Double, Double)) -- ˆ How to convert events into states.
  -> (s -> Picture) -- ˆ How to draw states.
  -> (a -> Picture) -- ˆ How to draw actions.
  -> ((s, Double, Double) -> Picture) -- ^ Rednerer
  -> IO ()


interactiveFSM initialState equalityCheck mapping eventConverter drawState drawAction renderer = solution
  where

     solution = activityOf initialState eventConverter renderer

renderPicture :: (Mode, Double, Double) -> Picture
renderPicture (mode, coordinates, _) = drawMode mode <> translated 2 0 (asSpaced 1.5 drawButton (buttons)) <> translated (-3) coordinates (solidRectangle 1 1)
  where
    buttons = returnAllValues mode (elevator mode) isEqual


main :: IO()
main = interactiveFSM initialWorld isEqual elevator handleWorld  drawMode drawButton renderPicture
