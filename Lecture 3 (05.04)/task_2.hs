{-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}
{-# LANGUAGE OverloadedStrings #-}
import CodeWorld

data Button = Up | Down | Stop
data Mode = Idle | MovingUp | MovingDown


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


main :: IO()
main = drawingOf (drawMode MovingDown)--(asSpaced 2 drawButton [Up, Down, Stop])
