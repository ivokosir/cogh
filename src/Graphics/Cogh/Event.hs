module Graphics.Cogh.Event
  ( WindowState (..)
  , pollEvents
  , WindowSize
  , Quit
  ) where

import Graphics.Cogh.Event.Internal
import Graphics.Cogh.Window.Internal
import qualified Graphics.Cogh.Key.Internal as Key
import qualified Graphics.Cogh.Mouse.Internal as Mouse
import qualified Graphics.Cogh.Joystick.Internal as Joystick

data WindowState = WindowState
  { keys :: [Key.Key]
  , mouseButtons :: [Mouse.Button]
  , mousePositions :: [Mouse.Position]
  , mouseScrolls :: [Mouse.Scroll]
  , joystickButtons :: [Joystick.Button]
  , joystickAxii :: [Joystick.Axis]
  , windowSizes :: [WindowSize]
  , quits :: [Quit]
  }

pollEvents :: Window -> IO WindowState
pollEvents w = do
  cPollEvents w

  WindowState
    <$> Key.getKeys w
    <*> Mouse.getButtons w
    <*> Mouse.getPositions w
    <*> Mouse.getScrolls w
    <*> Joystick.getButtons w
    <*> Joystick.getAxii w
    <*> getWindowSizes w
    <*> getQuits w


type WindowSize = (Int, Int)

getWindowSizes :: Window -> IO [WindowSize]
getWindowSizes = getEvents cGetWindowSizes castWindowSize

castWindowSize :: Ptr () -> IO WindowSize
castWindowSize cWindowSize = do
  w <- windowSizeW cWindowSize
  h <- windowSizeH cWindowSize
  return (fromIntegral w, fromIntegral h)


data Quit = Quit

getQuits :: Window -> IO [Quit]
getQuits = getEvents cGetQuits castQuit

castQuit :: Ptr () -> IO Quit
castQuit _ = return Quit


foreign import ccall unsafe "pollEvents" cPollEvents
  :: Window -> IO ()


foreign import ccall unsafe "getSizes" cGetWindowSizes
  :: Window -> IO (Ptr (Ptr ()))

foreign import ccall unsafe "sizeW" windowSizeW
  :: Ptr () -> IO CUInt

foreign import ccall unsafe "sizeH" windowSizeH
  :: Ptr () -> IO CUInt


foreign import ccall unsafe "getQuits" cGetQuits
  :: Window -> IO (Ptr (Ptr ()))
