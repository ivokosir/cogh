module Graphics.Cogh.WindowState
  ( WindowState (..)
  , toWorld
  , fromWorld
  , deltaTime
  , updateTime
  , initialWindowState
  ) where

import Data.Word
import Graphics.Cogh.Vector
import qualified Graphics.Cogh.Key.Internal as Key
import qualified Graphics.Cogh.Mouse.Internal as Mouse
import qualified Graphics.Cogh.Joystick.Internal as Joystick

data WindowState = WindowState
  { keys :: [Key.Key]
  , pressedKeys :: [Key.Code]
  , mouseButtons :: [Mouse.Button]
  , pressedMouseButtons :: [Mouse.Code]
  , mouseMotions :: [Mouse.Motion]
  , mousePosition :: Mouse.Position
  , mouseScrolls :: [Mouse.Scroll]
  , joysticks :: [Joystick.Joystick]
  , windowSizes :: [Pixel]
  , windowSize :: Pixel
  , quit :: Bool
  , previousTime :: Word32
  , time :: Word32
  } deriving (Eq, Show, Read)

toWorld :: WindowState -> Pixel -> Vector
toWorld ws point = Point (x*px/wx) (y*py/wy)
 where
  Point{x, y} = fromIntegral <$> point
  Point{x=wx, y=wy} = fromIntegral <$> windowSize ws
  Point{x=px, y=py} = fromIntegral <$> windowSize ws

fromWorld :: WindowState -> Vector -> Pixel
fromWorld ws vector = round <$> Point (x*wx/px) (y*wy/py)
 where
  Point{x, y} = vector
  Point{x=wx, y=wy} = fromIntegral <$> windowSize ws
  Point{x=px, y=py} = fromIntegral <$> windowSize ws

deltaTime :: WindowState -> Float
deltaTime ws = fromIntegral deltaMilisPositive / 1000
 where
  deltaMilis = time ws - previousTime ws
  deltaMilisPositive | deltaMilis > 0 = deltaMilis
                     | otherwise = 0

updateTime :: WindowState -> Word32 -> WindowState
updateTime ws newTime = ws
  { previousTime = time ws
  , time = newTime
  }

initialWindowState :: Word32 -> WindowState
initialWindowState initialTime =
  WindowState
    [] [] [] [] [] (Point 0 0)
    [] [] [] (Point 0 0) False
    initialTime initialTime
