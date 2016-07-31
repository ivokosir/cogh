module Graphics.Cogh.WindowState
  ( WindowState (..)
  , toWorld
  , fromWorld
  , deltaTime
  , updateTime
  , initialWindowState
  , getHoveredElement
  ) where

import Data.List
import Data.Word
import Graphics.Cogh.Element
import Graphics.Cogh.Matrix
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
  , elements :: [(Element, Matrix, Matrix)]
  }

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

updateTime :: WindowState -> IO WindowState
updateTime ws = do
  newTime <- cTime
  return $ ws
    { previousTime = time ws
    , time = newTime
    }

initialWindowState :: IO WindowState
initialWindowState = do
  initialTime <- cTime
  return $ WindowState
    [] [] [] [] [] (Point 0 0)
    [] [] [] (Point 0 0) False
    initialTime initialTime
    []

foreign import ccall unsafe "time" cTime
  :: IO Word32

getHoveredElement :: WindowState -> Maybe (Element, Vector)
getHoveredElement ws = esAndRelativeMouse
 where
  es = elements ws
  esWithMouse = toMouse <$> es
  toMouse (element, view, local) =
    ( element
    , mouseToMatrix view
    , mouseToMatrix local
    )
  mouseToMatrix matrix = dotVector (inverse matrix) mouseVector
  mouseVector = Point (2*(x mouse/x screen) - 1) (1 - 2*(y mouse/y screen))
  mouse = fromIntegral <$> mousePosition ws
  screen = fromIntegral <$> windowSize ws
  onlyInsideEs = find isInside esWithMouse
  isInside (element, _, Point {x, y}) =
    size element /= Point 0 0 &&
    x >= 0 && x <= 1 &&
    y >= 0 && y <= 1
  esAndRelativeMouse :: Maybe (Element, Vector)
  esAndRelativeMouse = toElementAndRelativeMouse <$> onlyInsideEs
  toElementAndRelativeMouse (e, m, _) = (e, m)