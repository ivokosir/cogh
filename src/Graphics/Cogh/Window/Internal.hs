module Graphics.Cogh.Window.Internal
  ( Window (..)
  , WindowPtr (..)
  , withCWindow
  , getWindowState
  , setWindowState
  ) where

import Data.IORef
import Graphics.Cogh.Window.CWindow
import Graphics.Cogh.WindowState

data Window = Window WindowPtr (IORef WindowState)

withCWindow :: Window -> (WindowPtr -> IO a) -> IO a
withCWindow (Window cWindow _) io = io cWindow

getWindowState :: Window -> IO WindowState
getWindowState (Window _ stateRef) = readIORef stateRef

setWindowState :: Window -> WindowState -> IO ()
setWindowState (Window _ stateRef) = writeIORef stateRef
