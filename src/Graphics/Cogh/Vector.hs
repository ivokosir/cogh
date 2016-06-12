module Graphics.Cogh.Vector where

data Point a = Point
  { x :: a
  , y :: a
  } deriving (Eq, Show, Read)

instance Num a => Num (Point a) where
  (Point x1 y1) + (Point x2 y2) = Point (x1 + x2) (y1 + y2)
  (Point x1 y1) - (Point x2 y2) = Point (x1 - x2) (y1 - y2)
  (Point x1 y1) * (Point x2 y2) = Point (x1 * x2) (y1 * y2)
  abs = fmap abs
  signum = fmap signum
  fromInteger x = Point (fromInteger x) (fromInteger x)

instance Functor Point where
  fmap f Point{x, y} = Point (f x) (f y)

vectorAngle :: RealFloat a => Point a -> a
vectorAngle Point{x, y} = atan2 x y

vectorScale :: Num a => a -> Point a -> Point a
vectorScale s Point{x, y} = Point (s * x) (s * y)

vectorLength :: Floating a => Point a -> a
vectorLength Point{x, y} = sqrt (x**2 + y**2)

setVectorLength :: Floating a => a -> Point a -> Point a
setVectorLength newLength p = vectorScale lengthRatio p
 where
  oldLength = vectorLength p
  lengthRatio = newLength / oldLength

normalize :: Floating a => Point a -> Point a
normalize = setVectorLength 1

type Pixel = Point Int

type Vector = Point Float
