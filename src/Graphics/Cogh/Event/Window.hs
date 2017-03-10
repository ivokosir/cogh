module Graphics.Cogh.Event.Window
  ( Event(..)
  , getEvents
  ) where

import Foreign.C
import Foreign.Ptr
import qualified Graphics.Cogh.Event.Helper as Helper
import Graphics.Cogh.Window.Internal
import Graphics.Cogh.Vector

data Event
  = Quit
  | WindowSize Pixel

getEvents :: Window -> IO [Event]
getEvents w = do
  windowSizes <- getWindowSizes w
  quit <- getQuit w
  return $ fmap WindowSize windowSizes ++ [Quit | quit]

getWindowSizes :: Window -> IO [Pixel]
getWindowSizes = Helper.getEvents cGetWindowSizes castWindowSize

castWindowSize :: Ptr () -> IO Pixel
castWindowSize cWindowSize = do
  w <- windowSizeW cWindowSize
  h <- windowSizeH cWindowSize
  return $ Point (fromIntegral w) (fromIntegral h)

getQuit :: Window -> IO Bool
getQuit w = Helper.cBool <$> cGetQuit w

foreign import ccall unsafe "getSizes" cGetWindowSizes ::
               Window -> IO (Ptr (Ptr ()))

foreign import ccall unsafe "sizeW" windowSizeW ::
               Ptr () -> IO CUInt

foreign import ccall unsafe "sizeH" windowSizeH ::
               Ptr () -> IO CUInt

foreign import ccall unsafe "getQuit" cGetQuit ::
               Window -> IO CInt
