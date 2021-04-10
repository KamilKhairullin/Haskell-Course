{-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}
{-# LANGUAGE OverloadedStrings #-}
import CodeWorld

data Button = Up | Down | Stop

template :: Picture
template = colored (light grey) (solidCircle 0.425) <> colored black (solidCircle 0.5)

drawButton :: Button -> Picture
drawButton Up = (scaled 0.8 0.8 (colored white (lettering  "\x1F809"))) <> template
drawButton Down = (scaled 0.8 0.8 (colored white (lettering  "\x1F80B"))) <> template
drawButton Stop = (scaled 0.3 0.3 (colored red (lettering  "STOP"))) <> template

asSpaced :: Double -> (a -> Picture) -> [a] -> Picture
asSpaced _ _ [] = blank
asSpaced range f (x:xs) = f x <> shifted (asSpaced range f xs)
  where
    shifted = translated range 0


main :: IO()
main = drawingOf (asSpaced 2 drawButton [Up, Down, Stop])
