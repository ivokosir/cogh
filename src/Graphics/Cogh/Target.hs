module Graphics.Cogh.Target
  ( Target(..)
  , toWorld
  , toTarget
  , isInside
  ) where

import Graphics.Cogh.Matrix
import Graphics.Cogh.Vector
import Lens.Micro

data Target =
  Target Matrix
         Matrix

toWorld :: Pixel -> Pixel -> Vector
toWorld windowSize p = vector (2 * (ratio ^. x) - 1) (1 - 2 * (ratio ^. y))
  where
    ratio = toVector windowSize / toVector p

toTarget :: Target -> Vector -> (Vector, Vector)
toTarget (Target view local) v = (toMatrix view, toMatrix local)
  where
    toMatrix matrix = dotVector (inverse matrix) v

isInside :: (Vector, Vector) -> Bool
isInside (_, v) =
  (v ^. x) >= 0 && (v ^. x) <= 1 && (v ^. y) >= 0 && (v ^. y) <= 1
