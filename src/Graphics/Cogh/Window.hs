module Graphics.Cogh.Window
  ( module Export
  , Window
  , newWindow
  , deleteWindow
  , refreshWindow
  ) where

import Graphics.Cogh.WindowState as Export hiding
  ( updateTime
  , initialWindowState
  )

import Data.IORef
import Data.Word
import Foreign.C
import Foreign.Ptr
import Graphics.Cogh.Element
import Graphics.Cogh.Event
import Graphics.Cogh.WindowState
import Graphics.Cogh.Window.Internal

newWindow :: String -> IO (Maybe Window)
newWindow title = do
  windowPtr <- withCString title cNewWindow
  if (\ (WindowPtr p) -> p) windowPtr /= nullPtr
    then do
      initialTime <- cTime
      stateRef <- newIORef $ initialWindowState initialTime
      return . Just $ Window windowPtr stateRef
    else return Nothing

foreign import ccall unsafe "newWindow" cNewWindow
  :: CString -> IO WindowPtr

foreign import ccall unsafe "deleteWindow" deleteWindow
  :: WindowPtr -> IO ()

refreshWindow :: Window -> Maybe Element -> IO WindowState
refreshWindow window mElement = do
  oldState <- getWindowState window
  stateWithEvents <- withCWindow window $ \ cWindow -> do
    case mElement of
      (Just element) -> renderRoot cWindow (windowSize oldState) element
      _ -> return ()
    pollEvents cWindow oldState
  newTime <- cTime
  let newState = updateTime stateWithEvents newTime
  setWindowState window newState
  return newState

foreign import ccall unsafe "time" cTime
  :: IO Word32
