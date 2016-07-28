module Graphics.Cogh.Event
  ( pollEvents
  ) where

import Graphics.Cogh.Button
import Graphics.Cogh.Event.Internal
import Graphics.Cogh.Vector
import Graphics.Cogh.WindowState
import Graphics.Cogh.Window.CWindow
import qualified Graphics.Cogh.Key.Internal as Key
import qualified Graphics.Cogh.Mouse.Internal as Mouse
import qualified Graphics.Cogh.Joystick.Internal as Joystick

pollEvents :: WindowPtr -> WindowState -> IO WindowState
pollEvents w oldState = do
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
      getPressedButtons Key.code newKeys (pressedKeys oldState)
    newPressedMouseButtons =
      getPressedButtons Mouse.code newMouseButtons (pressedMouseButtons oldState)

    newMousePosition =
      last $ mousePosition oldState : fmap Mouse.position newMouseMotions
    lastWindowSize = last $ windowSize oldState : newWindowSizes

    newState = oldState
      { keys = newKeys
      , pressedKeys = newPressedKeys
      , mouseButtons = newMouseButtons
      , pressedMouseButtons = newPressedMouseButtons
      , mouseMotions = newMouseMotions
      , mousePosition = newMousePosition
      , mouseScrolls = newMouseScrolls
      , joysticks = newJoysticks
      , windowSizes = newWindowSizes
      , windowSize = lastWindowSize
      , quit = newQuit
      }

  return newState


getWindowSizes :: WindowPtr -> IO [Pixel]
getWindowSizes = getEvents cGetWindowSizes castWindowSize

castWindowSize :: Ptr () -> IO Pixel
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
