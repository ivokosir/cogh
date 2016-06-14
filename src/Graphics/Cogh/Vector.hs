module Graphics.Cogh.Vector where

data Point a = Point
  { x :: a
  , y :: a
  } deriving (Eq, Show, Read)

type Angle = Float

type Pixel = Point Int

type Vector = Point Float

instance Num a => Num (Point a) where
  (Point x1 y1) + (Point x2 y2) = Point (x1 + x2) (y1 + y2)
  (Point x1 y1) - (Point x2 y2) = Point (x1 - x2) (y1 - y2)
  (Point x1 y1) * (Point x2 y2) = Point (x1 * x2) (y1 * y2)
  abs = fmap abs
  signum = fmap signum
  fromInteger x = Point (fromInteger x) (fromInteger x)

instance Functor Point where
  fmap f Point{x, y} = Point (f x) (f y)

vectorScale :: Float -> Vector -> Vector
vectorScale s Point{x, y} = Point (s * x) (s * y)

vectorAngle :: Vector -> Angle
vectorAngle Point{x, y} = atan2 x y

setVectorAngle :: Angle -> Vector -> Vector
setVectorAngle angle v = vectorRotate (angle-oldAngle) v
 where
  oldAngle = vectorAngle v

vectorRotate :: Angle -> Vector -> Vector
vectorRotate angle Point{x, y} = Point { x = x', y = y' }
 where
  cosA = cos angle
  sinA = sin angle
  x' = x*cosA - y*sinA
  y' = x*sinA + y*cosA

vectorLength :: Vector -> Float
vectorLength Point{x, y} = sqrt (x**2 + y**2)

setVectorLength :: Float -> Vector -> Vector
setVectorLength newLength p
  | oldLength == 0 = Point 0 0
  | otherwise = vectorScale lengthRatio p
 where
  oldLength = vectorLength p
  lengthRatio = newLength / oldLength

vectorNormalize :: Vector -> Vector
vectorNormalize = setVectorLength 1
