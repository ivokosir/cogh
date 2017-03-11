module Graphics.Cogh.Window
  ( Window
  , newWindow
  , deleteWindow
  , render
  ) where

import Foreign.C
import Foreign.Ptr
import Graphics.Cogh.Element.Internal
import qualified Graphics.Cogh.Vector as V
import Graphics.Cogh.Window.Internal

newWindow :: String -> IO (Maybe Window)
newWindow title = do
  window <- withCString title cNewWindow
  if (\(Window ptr) -> ptr) window /= nullPtr
    then return $ Just window
    else return Nothing

foreign import ccall unsafe "newWindow" cNewWindow ::
               CString -> IO Window

foreign import ccall unsafe "deleteWindow" deleteWindow ::
               Window -> IO ()

render :: Window -> V.Pixel -> Element a -> IO [a]
render window size element = do
  let (events, renderFunction) = normalize size element
  renderFunction window
  sequence (fmap (\event -> event window) events)
