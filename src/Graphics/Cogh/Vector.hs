module Graphics.Cogh.Vector
  ( Point
  , point
  , x
  , y
  , setX
  , setY
  , Pixel
  , pixel
  , Vector
  , vector
  , angle
  , setAngle
  , rotate
  , length
  , setLength
  , normalize
  ) where

import Prelude hiding (length)

data Point a =
  Point a
        a
  deriving (Eq, Show, Read)

point :: a -> a -> Point a
point = Point

x :: Point a -> a
x (Point x' _) = x'

y :: Point a -> a
y (Point _ y') = y'

setX :: Point a -> a -> Point a
setX p x' = point x' (y p)

setY :: Point a -> a -> Point a
setY p y' = point (x p) y'

type Pixel = Point Int

pixel :: Int -> Int -> Pixel
pixel = point

type Vector = Point Float

vector :: Float -> Float -> Vector
vector = point

instance Num a =>
         Num (Point a) where
  p1 + p2 = point (x p1 + x p2) (y p1 + y p2)
  p1 - p2 = point (x p1 - x p2) (y p1 - y p2)
  p1 * p2 = point (x p1 * x p2) (y p1 * y p2)
  abs = fmap abs
  signum = fmap signum
  fromInteger a = point (fromInteger a) (fromInteger a)

instance Functor Point where
  fmap f p = point (f (x p)) (f (y p))

scale :: Float -> Vector -> Vector
scale s p = point (s * x p) (s * y p)

angle :: Vector -> Float
angle p = atan2 (y p) (x p)

setAngle :: Float -> Vector -> Vector
setAngle newAngle v = rotate (newAngle - oldAngle) v
  where
    oldAngle = angle v

rotate :: Float -> Vector -> Vector
rotate diff p = point x' y'
  where
    c = cos diff
    s = sin diff
    x' = x p * c - y p * s
    y' = x p * s + y p * c

length :: Vector -> Float
length p = sqrt (x p ** 2 + y p ** 2)

setLength :: Float -> Vector -> Vector
setLength newLength p
  | oldLength == 0 = Point 0 0
  | otherwise = scale lengthRatio p
  where
    oldLength = length p
    lengthRatio = newLength / oldLength

normalize :: Vector -> Vector
normalize = setLength 1
