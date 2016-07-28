module Graphics.Cogh.Window
  ( module Export
  , Window
  , newWindow
  , deleteWindow
  , updateWindow
  , refreshWindow
  ) where

import Graphics.Cogh.WindowState as Export hiding
  ( updateTime
  , initialWindowState
  )

import Data.IORef
import Foreign.C
import Foreign.Ptr
import Graphics.Cogh.Element.Internal
import Graphics.Cogh.Event
import Graphics.Cogh.WindowState
import Graphics.Cogh.Window.Internal

newWindow :: String -> IO (Maybe Window)
newWindow title = do
  windowPtr <- withCString title cNewWindow
  if (\ (WindowPtr p) -> p) windowPtr /= nullPtr
    then do
      stateRef <- newIORef =<< initialWindowState
      return . Just $ Window windowPtr stateRef
    else return Nothing

foreign import ccall unsafe "newWindow" cNewWindow
  :: CString -> IO WindowPtr

foreign import ccall unsafe "deleteWindow" deleteWindow
  :: WindowPtr -> IO ()

updateWindow :: Window -> IO WindowState
updateWindow window = do
  oldState <- getWindowState window
  stateWithEvents <- withCWindow window $ \ cWindow ->
    pollEvents cWindow oldState
  newState <- updateTime stateWithEvents
  setWindowState window newState
  return newState

refreshWindow :: Window -> Element -> IO WindowState
refreshWindow window element = do
  state <- getWindowState window
  newElements <- withCWindow window $ \ cWindow ->
      renderRoot cWindow (windowSize state) element
  setWindowState window state { elements = newElements }
  updateWindow window
