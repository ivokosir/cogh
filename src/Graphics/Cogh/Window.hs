module Graphics.Cogh.Window
  ( Window
  , newWindow
  , deleteWindow
  , time
  ) where

import Data.Word
import Graphics.Cogh.CommonFFI

newWindow :: String -> IO (Maybe Window)
newWindow title = do
  w <- withCString title newWindow'
  if (\ (Window p) -> p) w /= nullPtr
    then return (Just w)
    else return Nothing

foreign import ccall unsafe "newWindow" newWindow'
  :: CString -> IO Window

foreign import ccall unsafe "deleteWindow" deleteWindow
  :: Window -> IO ()

foreign import ccall unsafe "time" time
  :: IO (Word32)
