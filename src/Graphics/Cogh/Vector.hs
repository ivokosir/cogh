module Graphics.Cogh.Vector
  ( Point
  , point
  , Pixel
  , pixel
  , Vector
  , vector
  , x
  , y
  , angle
  , length
  , normalize
  ) where

import Lens.Micro
import Lens.Micro.TH
import Prelude hiding (length)

data Point a = Point
  { _x :: a
  , _y :: a
  } deriving (Eq, Show, Read)

makeLenses ''Point

point :: a -> a -> Point a
point = Point

type Pixel = Point Int

pixel :: Int -> Int -> Pixel
pixel = point

type Vector = Point Float

vector :: Float -> Float -> Vector
vector = point

instance Num a =>
         Num (Point a) where
  p1 + p2 = point (p1 ^. x + p2 ^. x) (p1 ^. y + p2 ^. y)
  p1 - p2 = point (p1 ^. x - p2 ^. x) (p1 ^. y - p2 ^. y)
  p1 * p2 = point (p1 ^. x * p2 ^. x) (p1 ^. y * p2 ^. y)
  abs = fmap abs
  signum = fmap signum
  fromInteger a = point (fromInteger a) (fromInteger a)

instance Functor Point where
  fmap f p = point (f (p ^. x)) (f (p ^. y))

scale :: Float -> Vector -> Vector
scale s v = point (s * v ^. x) (s * v ^. y)

angle :: Lens' Vector Float
angle = lens viewAngle setAngle

viewAngle :: Vector -> Float
viewAngle v = atan2 (v ^. y) (v ^. x)

setAngle :: Vector -> Float -> Vector
setAngle v newAngle = rotate (newAngle - oldAngle) v
  where
    oldAngle = viewAngle v

rotate :: Float -> Vector -> Vector
rotate diff v = point x' y'
  where
    c = cos diff
    s = sin diff
    x' = v ^. x * c - v ^. y * s
    y' = v ^. x * s + v ^. y * c

length :: Lens' Vector Float
length = lens viewLength setLength

viewLength :: Vector -> Float
viewLength v = sqrt ((v ^. x) ** 2 + (v ^. y) ** 2)

setLength :: Vector -> Float -> Vector
setLength v newLength
  | oldLength == 0 = Point 0 0
  | otherwise = scale lengthRatio v
  where
    oldLength = viewLength v
    lengthRatio = newLength / oldLength

normalize :: Vector -> Vector
normalize = length .~ 1
