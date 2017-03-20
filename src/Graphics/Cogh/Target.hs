module Graphics.Cogh.Target
  ( Target(..)
  , viewMatrix
  , localMatrix
  ) where

import Graphics.Cogh.Matrix

data Target =
  Target Matrix
         Matrix

viewMatrix :: Target -> Matrix
viewMatrix (Target view _) = view

localMatrix :: Target -> Matrix
localMatrix (Target _ local) = local
