module Graphics.Cogh.Window
  ( Window
  , WindowState (..)
  , newWindow
  , deleteWindow
  , fromWorld
  , toWorld
  , deltaTime
  ) where

import Data.IORef
import Foreign.C
import Foreign.Ptr
import Graphics.Cogh.Event

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
