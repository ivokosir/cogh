module Graphics.Cogh.Target
  ( Target
  , target
  , viewMatrix
  , localMatrix
  ) where

import Graphics.Cogh.Matrix

data Target =
  Target Matrix
         Matrix

target :: Matrix -> Matrix -> Target
target = Target

viewMatrix :: Target -> Matrix
viewMatrix (Target view _) = view

localMatrix :: Target -> Matrix
localMatrix (Target _ local) = local
