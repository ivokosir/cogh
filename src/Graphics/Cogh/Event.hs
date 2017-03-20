module Graphics.Cogh.Event
  ( Events(..)
  , pollEvents
  ) where

import Foreign.C
import Foreign.Ptr
import Graphics.Cogh.Event.Helper
import qualified Graphics.Cogh.Event.Keyboard as Keyboard
import qualified Graphics.Cogh.Event.Mouse as Mouse
import Graphics.Cogh.Vector
import Graphics.Cogh.Window.Internal

data Events = Events
  { keys :: [Keyboard.Key]
  , mouseButtons :: [Mouse.Button]
  , mouseMotions :: [Mouse.Motion]
  , mouseScrolls :: [Pixel]
  , windowSizes :: [Pixel]
  , quits :: [()]
  } deriving (Eq, Show, Read)

pollEvents :: Window -> IO Events
pollEvents w = do
  cPollEvents w
  keyboardEvents' <- Keyboard.getKeys w
  mouseButtons' <- Mouse.getButtons w
  mouseMotions' <- Mouse.getMotions w
  mouseScrolls' <- Mouse.getScrolls w
  windowSizes' <- getWindowSizes w
  quit <- getQuit w
  return $
    Events
      keyboardEvents'
      mouseButtons'
      mouseMotions'
      mouseScrolls'
      windowSizes'
      [() | quit]

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
