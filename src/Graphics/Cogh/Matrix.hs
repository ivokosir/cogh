module Graphics.Cogh.Matrix
  ( Matrix
  , Angle, Point, Position, Size, Scale, Origin
  , identity
  , translation
  , scaling
  , rotation
  , projection
  , model
  , dot
  , withMatrixPtr
  ) where

import Foreign.Marshal.Array
import Foreign.Ptr

type Angle = Float
type Point = (Float, Float)
type Position = (Float, Float)
type Size = (Float, Float)
type Scale = (Float, Float)
type Origin = (Float, Float)

data Matrix = Matrix
  Float Float Float
  Float Float Float
  -- assume c f i are 0 0 1

instance Monoid Matrix where
  mempty = identity
  mappend = dot

identity :: Matrix
identity = Matrix
  1 0 0
  0 1 0

translation :: Point -> Matrix
translation (x, y) = Matrix
  1 0 x
  0 1 y

scaling :: Point -> Matrix
scaling (x, y) = Matrix
  x 0 0
  0 y 0

rotation :: Angle -> Matrix
rotation angle = Matrix
  c   s 0
 (-s) c 0
 where
  s = sin angle
  c = cos angle

projection :: Point -> Matrix
projection (w, h) = Matrix
  (2/w) 0 (-1)
  0 (-2/h) 1

model :: Position -> Scale -> Angle -> Matrix
model (x, y) (w, h) angle = Matrix
  (c*w) (s*h) x
  (-s*w) (c*h) y
 where
  s = sin angle
  c = cos angle

dot :: Matrix -> Matrix -> Matrix
dot
  (Matrix
    a1 d1 g1
    b1 e1 h1
  )
  (Matrix
    a2 d2 g2
    b2 e2 h2
  ) = Matrix
    (a1*a2 + d1*b2) (a1*d2 + d1*e2) (a1*g2 + d1*h2 + g1)
    (b1*a2 + e1*b2) (b1*d2 + e1*e2) (b1*g2 + e1*h2 + h1)

withMatrixPtr :: Matrix -> (Ptr Float -> IO a) -> IO a
withMatrixPtr
  (Matrix
    a d g
    b e h
  ) = withArray [a, b, 0, d, e, 0, g, h, 1]
