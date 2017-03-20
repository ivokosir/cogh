module Graphics.Cogh.Window
  ( Window
  , newWindow
  , deleteWindow
  , update
  ) where

import Data.Foldable
import Data.Function
import Foreign.C
import Foreign.Ptr
import Graphics.Cogh.Element.Internal
import Graphics.Cogh.Event
import Graphics.Cogh.Matrix
import Graphics.Cogh.Render
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

update :: Window -> a -> (a -> Element (a -> a)) -> (a -> Bool) -> IO a
update window initial view exit = updateLoop initial (V.pixel 0 0)
  where
    updateLoop old windowSize =
      if exit old
        then return old
        else do
          events <- pollEvents window
          let newSize = last (windowSize : windowSizes events)
              matrix = projection $ fromIntegral <$> newSize
              element = view old
              (updates, renderAll) = normalize window events element matrix
          clear window
          renderAll
          swapBuffers window
          new <- foldl' (&) old . concat <$> sequence updates
          updateLoop new newSize
