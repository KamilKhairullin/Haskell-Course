{-# LANGUAGE OverloadedStrings #-}
import CodeWorld

tree :: Double -> Picture
tree 0 = blank
tree len = segment <> leftBranch <> rightBranch
  where
    segment = polyline [(0, 0), (0, len)]
    leftBranch = translated 0 len (rotated (pi/8) (tree (len - 1)))
    rightBranch = translated 0 len (rotated (-pi/8) (tree (len - 1)))

main :: IO ()
main = drawingOf (tree 5)
