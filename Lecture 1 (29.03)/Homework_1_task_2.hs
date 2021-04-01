{-# LANGUAGE OverloadedStrings #-}
import CodeWorld

tree :: Double -> Double -> Picture
tree 0 thickness = (colored green (solidCircle 0.3))
tree len thickness= segment <> leftBranch <> rightBranch
  where
    segment = solidPolygon [(-thickness, 0), (thickness, 0), (thickness / 2, len), (-(thickness / 2), len)]
    leftBranch = translated 0 len (rotated (pi/8) (tree (len - 1) (thickness / 2) ))
    rightBranch = translated 0 len (rotated (-pi/8) (tree (len - 1) (thickness / 2) ))

main :: IO ()
main = drawingOf (tree 7 2) 
