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
  , mousePositions :: [Mouse.Position]
  , mousePosition :: Mouse.Position
  , mouseScrolls :: [Mouse.Scroll]
  , joysticks :: [Joystick.Joystick]
  , windowSizes :: [WindowSize]
  , windowSize :: WindowSize
  , quit :: Bool
  }

initialWindowState :: WindowState
initialWindowState = WindowState
  [] [] [] [] [] (0, 0) [] [] [] (0, 0) False

pollEvents :: Window -> IO WindowState
pollEvents window@(Window _ stateRef) = withCWindow window $ \ w -> do
  oldState <- readIORef stateRef
  cPollEvents w

  newKeys           <- Key.getKeys w
  newMouseButtons   <- Mouse.getButtons w
  newMousePositions <- Mouse.getPositions w
  newMouseScrolls   <- Mouse.getScrolls w
  newJoysticks      <- Joystick.getJoysticks w (joysticks oldState)
  newWindowSizes    <- getWindowSizes w
  newQuit           <- getQuit w

  let
    newPressedKeys = getPressedButtons Button
      { isPressed = (/=) Key.Release . Key.state
      , code = Key.code
      , newButtons = newKeys
      , oldPressedButtons = pressedKeys oldState
      }
    newPressedMouseButtons = getPressedButtons Button
      { isPressed = (==) Mouse.Press . Mouse.state
      , code = Mouse.code
      , newButtons = newMouseButtons
      , oldPressedButtons = pressedMouseButtons oldState
      }
    lastMousePosition = last $ mousePosition oldState : newMousePositions
    lastWindowSize = last $ windowSize oldState : newWindowSizes
    newState = WindowState
      newKeys
      newPressedKeys
      newMouseButtons
      newPressedMouseButtons
      newMousePositions
      lastMousePosition
      newMouseScrolls
      newJoysticks
      newWindowSizes
      lastWindowSize
      newQuit

  writeIORef stateRef newState
  return newState

data Button a b = Button
  { isPressed :: a -> Bool
  , code :: a -> b
  , newButtons :: [a]
  , oldPressedButtons :: [a]
  }
getPressedButtons :: (Eq b) => Button a b -> [a]
getPressedButtons (Button isPressed' code' newButtons' oldPressedButtons') =
  newPressedButtons ++ filter isInNewButtons oldPressedButtons'
 where
  isSame a b = code' a == code' b
  isInNewButtons button = not $ any (isSame button) newPressedButtons
  newPressedButtons = filter isPressed' newButtons'

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
