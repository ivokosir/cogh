module Graphics.Cogh.Matrix
  ( Matrix(..)
  , Position
  , Size
  , Scale
  , Origin
  , identity
  , translation
  , scaling
  , rotation
  , projection
  , model
  , dot
  , inverse
  , dotVector
  ) where

import qualified Graphics.Cogh.Vector as V
import Lens.Micro

type Position = V.Vector

type Size = V.Vector

type Scale = V.Vector

type Origin = V.Vector

data Matrix =
  Matrix Float
         Float
         Float
         Float
         Float
         Float
  -- assume c f i are 0 0 1

instance Monoid Matrix where
  mempty = identity
  mappend = dot

identity :: Matrix
identity = Matrix 1 0 0 0 1 0

translation :: V.Vector -> Matrix
translation v = Matrix 1 0 x 0 1 y
  where
    x = v ^. V.x
    y = v ^. V.y

scaling :: V.Vector -> Matrix
scaling v = Matrix x 0 0 0 y 0
  where
    x = v ^. V.x
    y = v ^. V.y

rotation :: Float -> Matrix
rotation angle = Matrix c (-s) 0 s c 0
  where
    s = sin angle
    c = cos angle

projection :: V.Vector -> Matrix
projection v = Matrix (2 / w) 0 (-1) 0 (-2 / h) 1
  where
    w = v ^. V.x
    h = v ^. V.y

model :: Position -> Scale -> Float -> Matrix
model position scale angle = Matrix (c * w) (-s * h) x (s * w) (c * h) y
  where
    s = sin angle
    c = cos angle
    x = position ^. V.x
    y = position ^. V.y
    w = scale ^. V.x
    h = scale ^. V.y

dot :: Matrix -> Matrix -> Matrix
dot (Matrix a1 d1 g1 b1 e1 h1) (Matrix a2 d2 g2 b2 e2 h2) =
  Matrix
    (a1 * a2 + d1 * b2)
    (a1 * d2 + d1 * e2)
    (a1 * g2 + d1 * h2 + g1)
    (b1 * a2 + e1 * b2)
    (b1 * d2 + e1 * e2)
    (b1 * g2 + e1 * h2 + h1)

inverse :: Matrix -> Matrix
inverse (Matrix a d g b e h) = Matrix a' d' g' b' e' h'
  where
    det = a * e - b * d
    invdet = 1 / det
    a' = e * invdet
    b' = -b * invdet
    d' = -d * invdet
    e' = a * invdet
    g' = (d * h - g * e) * invdet
    h' = (g * b - a * h) * invdet

dotVector :: Matrix -> V.Vector -> V.Vector
dotVector (Matrix a d g b e h) v =
  V.vector (a * x + d * y + g) (b * x + e * y + h)
  where
    x = v ^. V.x
    y = v ^. V.y
