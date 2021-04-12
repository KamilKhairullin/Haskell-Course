{-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}
{-# LANGUAGE OverloadedStrings #-}
import CodeWorld

data Button = Up | Down | Stop deriving (Eq, Show)
data Mode = Idle | MovingUp | MovingDown deriving (Eq, Show)

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

initialWorld :: Mode
initialWorld = Idle

handleWorld :: Event -> Mode -> Mode
handleWorld (KeyPress "Up") currentMode = applyAction (Just Up) isEqual elevator currentMode
handleWorld (KeyPress "Down") currentMode = applyAction (Just Down) isEqual elevator currentMode
handleWorld (KeyPress " ") currentMode = applyAction (Just Stop) isEqual elevator currentMode
handleWorld _anyEvent currentMode = currentMode


interactiveFSM :: s -- ˆ Initial state.
  -> (a -> a -> Bool) -- ˆ Action equality test.
  -> (s -> [(a, s)]) -- ˆ State transitions.
  -> (Event -> s -> s) -- ˆ How to convert events into states.
  -> (s -> Picture) -- ˆ How to draw states.
  -> (a -> Picture) -- ˆ How to draw actions.
  -> IO ()

drawActions :: [a] -> (a -> Picture) -> Picture
drawActions [] f = blank
drawActions (x : xs) f = (f x) <> translated 2 0 (drawActions xs f)

interactiveFSM initialState isEqual mapping eventConverter drawState drawAction = solution
  where
     solution = activityOf initialState eventConverter renderedPicture
     renderedPicture state = drawState state <> translated 3 0 (drawActions (buttons state) drawAction)
     buttons state = returnAllValues state (mapping state) isEqual
main :: IO()
main = interactiveFSM initialWorld isEqual elevator handleWorld  drawMode drawButton
