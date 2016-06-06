module Graphics.Cogh.Event
  ( WindowState (..)
  , Window (..)
  , WindowPtr (..)
  , withCWindow
  , getWindowState
  , initialWindowState
  , pollEvents
  , WindowSize
  ) where

import Data.IORef
import Graphics.Cogh.Button
import Graphics.Cogh.Event.Internal
import Graphics.Cogh.Window.Internal
import qualified Graphics.Cogh.Key.Internal as Key
import qualified Graphics.Cogh.Mouse.Internal as Mouse
import qualified Graphics.Cogh.Joystick.Internal as Joystick

data Window = Window WindowPtr (IORef WindowState)

withCWindow :: Window -> (WindowPtr -> IO a) -> IO a
withCWindow (Window cWindow _) io = io cWindow

getWindowState :: Window -> IO WindowState
getWindowState (Window _ stateRef) = readIORef stateRef

data WindowState = WindowState
  { keys :: [Key.Key]
  , pressedKeys :: [Key.Key]
  , mouseButtons :: [Mouse.Button]
  , pressedMouseButtons :: [Mouse.Button]
  , mouseMotions :: [Mouse.Motion]
  , mousePosition :: (Int, Int)
  , mouseScrolls :: [Mouse.Scroll]
  , joysticks :: [Joystick.Joystick]
  , windowSizes :: [WindowSize]
  , windowSize :: WindowSize
  , quit :: Bool
  } deriving (Eq, Show, Read)

initialWindowState :: WindowState
initialWindowState = WindowState
  [] [] [] [] [] (0, 0) [] [] [] (0, 0) False

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

  let
    newPressedKeys =
      getPressedButtons newKeys (pressedKeys oldState)
    newPressedMouseButtons =
      getPressedButtons newMouseButtons (pressedMouseButtons oldState)
    newMousePosition = last $ mousePosition oldState : fmap Mouse.position newMouseMotions
    lastWindowSize = last $ windowSize oldState : newWindowSizes
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

  writeIORef stateRef newState
  return newState

type WindowSize = (Int, Int)

getWindowSizes :: WindowPtr -> IO [WindowSize]
getWindowSizes = getEvents cGetWindowSizes castWindowSize

castWindowSize :: Ptr () -> IO WindowSize
castWindowSize cWindowSize = do
  w <- windowSizeW cWindowSize
  h <- windowSizeH cWindowSize
  return (fromIntegral w, fromIntegral h)


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
