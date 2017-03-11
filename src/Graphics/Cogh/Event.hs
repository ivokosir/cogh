module Graphics.Cogh.Event
  ( Event
  , pollEvents
  ) where

import Foreign.C
import Foreign.Ptr
import Graphics.Cogh.Event.Helper
import qualified Graphics.Cogh.Event.Keyboard as Keyboard
import qualified Graphics.Cogh.Event.Mouse as Mouse
import Graphics.Cogh.Vector
import Graphics.Cogh.Window.Internal

data Event
  = Key Keyboard.Key
  | MouseButton Mouse.Button
  | MouseMotion Mouse.Motion
  | MouseScroll Pixel
  | WindowSize Pixel
  | Quit

pollEvents :: Window -> IO [Event]
pollEvents w = do
  cPollEvents w
  keyboardEvents <- Keyboard.getKeys w
  mouseButtons <- Mouse.getButtons w
  mouseMotions <- Mouse.getMotions w
  mouseScrolls <- Mouse.getScrolls w
  windowSizes <- getWindowSizes w
  quit <- getQuit w
  return $
    concat
      [ fmap Key keyboardEvents
      , fmap MouseButton mouseButtons
      , fmap MouseMotion mouseMotions
      , fmap MouseScroll mouseScrolls
      , fmap WindowSize windowSizes
      , [Quit | quit]
      ]

getWindowSizes :: Window -> IO [Pixel]
getWindowSizes = getEvents cGetWindowSizes castWindowSize

castWindowSize :: Ptr () -> IO Pixel
castWindowSize cWindowSize = do
  w <- windowSizeW cWindowSize
  h <- windowSizeH cWindowSize
  return $ pixel (fromIntegral w) (fromIntegral h)

getQuit :: Window -> IO Bool
getQuit w = cBool <$> cGetQuit w

foreign import ccall unsafe "pollEvents" cPollEvents ::
               Window -> IO ()

foreign import ccall unsafe "getSizes" cGetWindowSizes ::
               Window -> IO (Ptr (Ptr ()))

foreign import ccall unsafe "sizeW" windowSizeW ::
               Ptr () -> IO CUInt

foreign import ccall unsafe "sizeH" windowSizeH ::
               Ptr () -> IO CUInt

foreign import ccall unsafe "getQuit" cGetQuit :: Window -> IO CInt
