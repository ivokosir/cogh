module Graphics.Cogh.Event
  ( WindowState (..)
  , Window (..)
  , WindowPtr (..)
  , toWorld
  , fromWorld
  , deltaTime
  , withCWindow
  , getWindowState
  , initialWindowState
  , pollEvents
  , WindowSize
  ) where

import Data.IORef
import Data.Word
import Graphics.Cogh.Button
import Graphics.Cogh.Event.Internal
import Graphics.Cogh.Vector
import Graphics.Cogh.Window.Internal
import qualified Graphics.Cogh.Key.Internal as Key
import qualified Graphics.Cogh.Mouse.Internal as Mouse
import qualified Graphics.Cogh.Joystick.Internal as Joystick

data Window = Window WindowPtr (IORef WindowState)

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

withCWindow :: Window -> (WindowPtr -> IO a) -> IO a
withCWindow (Window cWindow _) io = io cWindow

getWindowState :: Window -> IO WindowState
getWindowState (Window _ stateRef) = readIORef stateRef

data WindowState = WindowState
  { keys :: [Key.Key]
  , pressedKeys :: [Key.Code]
  , mouseButtons :: [Mouse.Button]
  , pressedMouseButtons :: [Mouse.Code]
  , mouseMotions :: [Mouse.Motion]
  , mousePosition :: Mouse.Position
  , mouseScrolls :: [Mouse.Scroll]
  , joysticks :: [Joystick.Joystick]
  , windowSizes :: [WindowSize]
  , windowSize :: WindowSize
  , quit :: Bool
  , previousTime :: Word32
  , time :: Word32
  } deriving (Eq, Show, Read)

deltaTime :: WindowState -> Float
deltaTime ws = fromIntegral deltaMilisPositive / 1000
 where
  deltaMilis = time ws - previousTime ws
  deltaMilisPositive | deltaMilis > 0 = deltaMilis
                     | otherwise = 0

initialWindowState :: IO WindowState
initialWindowState = do
  newTime <- cTime
  return $ WindowState
    [] [] [] [] [] (Point 0 0)
    [] [] [] (Point 0 0) False
    newTime newTime

pollEvents :: Window -> IO WindowState
pollEvents window@(Window _ stateRef) = withCWindow window $ \ w -> do
  oldState <- readIORef stateRef
  cPollEvents w

  newKeys         <- Key.getKeys w
  newMouseButtons <- Mouse.getButtons w
  newMouseMotions <- Mouse.getMotions w
  newMouseScrolls <- Mouse.getScrolls w
  newJoysticks    <- Joystick.getJoysticks w (joysticks oldState)
  newWindowSizes  <- getWindowSizes w
  newQuit         <- getQuit w
  newTime         <- cTime

  let
    newPressedKeys =
      getPressedButtons Key.code newKeys (pressedKeys oldState)
    newPressedMouseButtons =
      getPressedButtons Mouse.code newMouseButtons (pressedMouseButtons oldState)

    newMousePosition =
      last $ mousePosition oldState : fmap Mouse.position newMouseMotions
    lastWindowSize = last $ windowSize oldState : newWindowSizes
    oldTime = time oldState

    newState = WindowState
      newKeys
      newPressedKeys
      newMouseButtons
      newPressedMouseButtons
      newMouseMotions
      newMousePosition
      newMouseScrolls
      newJoysticks
      newWindowSizes
      lastWindowSize
      newQuit
      oldTime
      newTime

  writeIORef stateRef newState
  return newState

type WindowSize = Pixel

getWindowSizes :: WindowPtr -> IO [WindowSize]
getWindowSizes = getEvents cGetWindowSizes castWindowSize

castWindowSize :: Ptr () -> IO WindowSize
castWindowSize cWindowSize = do
  w <- windowSizeW cWindowSize
  h <- windowSizeH cWindowSize
  return $ Point (fromIntegral w) (fromIntegral h)


getQuit :: WindowPtr -> IO Bool
getQuit w = cBool <$> cGetQuit w


foreign import ccall unsafe "pollEvents" cPollEvents
  :: WindowPtr -> IO ()


foreign import ccall unsafe "getSizes" cGetWindowSizes
  :: WindowPtr -> IO (Ptr (Ptr ()))

foreign import ccall unsafe "sizeW" windowSizeW
  :: Ptr () -> IO CUInt

foreign import ccall unsafe "sizeH" windowSizeH
  :: Ptr () -> IO CUInt


foreign import ccall unsafe "getQuit" cGetQuit
  :: WindowPtr -> IO CInt


foreign import ccall unsafe "time" cTime
  :: IO Word32
