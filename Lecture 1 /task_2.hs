{-# LANGUAGE OverloadedStrings #-}
import CodeWorld

-- |Recursive implementation of fractal tree given length of recursion and thickness of tree
tree :: Double -> Double -> Picture
tree 0 thickness = (colored green (solidCircle 0.3))
tree len thickness= segment <> leftBranch <> rightBranch
  where
    newThickness = thickness * 2 / 3
    segment = solidPolygon [(-thickness, 0), (thickness, 0), (newThickness, len), (-(newThickness), len)]
    leftBranch = translated 0 len (rotated (pi/8) (tree (len - 1) newThickness ))
    rightBranch = translated 0 len (rotated (-pi/8) (tree (len - 1) newThickness))

main :: IO()
main = drawingOf (tree 10 2)