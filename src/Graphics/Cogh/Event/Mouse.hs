module Graphics.Cogh.Event.Mouse
  ( Button
  , code
  , state
  , Code(..)
  , State(..)
  , Motion
  , position
  , motion
  , getButtons
  , getMotions
  , getScrolls
  ) where

import Foreign.C
import Foreign.Ptr
import Graphics.Cogh.Event.Helper
import Graphics.Cogh.Vector (Pixel, pixel)
import Graphics.Cogh.Window.Internal

import Prelude hiding (Left, Right)

data Button =
  Button Code
         State
  deriving (Eq, Read, Show)

code :: Button -> Code
code (Button c _) = c

state :: Button -> State
state (Button _ s) = s

data Code
  = Left
  | Middle
  | Right
  | Other Int
  deriving (Eq, Read, Show)

data State
  = Press
  | Release
  deriving (Eq, Read, Show)

getButtons :: Window -> IO [Button]
getButtons = getEvents cGetButtons castButton

castButton :: Ptr () -> IO Button
castButton cButton = do
  ccode <- buttonCode cButton
  isLeft <- buttonIsLeft cButton
  isMiddle <- buttonIsMiddle cButton
  isRight <- buttonIsRight cButton
  isPress <- buttonIsPress cButton
  let code'
        | cBool isLeft = Left
        | cBool isMiddle = Middle
        | cBool isRight = Right
        | otherwise = Other (fromIntegral ccode)
  return $
    Button
      code'
      (if cBool isPress
         then Press
         else Release)

data Motion =
  Motion Pixel
         Pixel
  deriving (Eq, Show, Read)

position :: Motion -> Pixel
position (Motion p _) = p

motion :: Motion -> Pixel
motion (Motion _ m) = m

getMotions :: Window -> IO [Motion]
getMotions = getEvents cGetMotions castMotion

castMotion :: Ptr () -> IO Motion
castMotion cMotion = do
  positionX <- motionPositionX cMotion
  positionY <- motionPositionY cMotion
  cMotionX <- motionMotionX cMotion
  cMotionY <- motionMotionY cMotion
  return $
    Motion
      (pixel (fromIntegral positionX) (fromIntegral positionY))
      (pixel (fromIntegral cMotionX) (fromIntegral cMotionY))

getScrolls :: Window -> IO [Pixel]
getScrolls = getEvents cGetScrolls castScroll

castScroll :: Ptr () -> IO Pixel
castScroll cScroll = do
  x <- scrollX cScroll
  y <- scrollY cScroll
  return $ pixel (fromIntegral x) (fromIntegral y)

foreign import ccall unsafe "getMouseButtons" cGetButtons ::
               Window -> IO (Ptr (Ptr ()))

foreign import ccall unsafe "mouseButtonIsLeft" buttonIsLeft ::
               Ptr () -> IO CInt

foreign import ccall unsafe "mouseButtonIsMiddle" buttonIsMiddle ::
               Ptr () -> IO CInt

foreign import ccall unsafe "mouseButtonIsRight" buttonIsRight ::
               Ptr () -> IO CInt

foreign import ccall unsafe "mouseButtonIsPress" buttonIsPress ::
               Ptr () -> IO CInt

foreign import ccall unsafe "mouseButtonCode" buttonCode ::
               Ptr () -> IO CUInt

foreign import ccall unsafe "getMouseMotions" cGetMotions ::
               Window -> IO (Ptr (Ptr ()))

foreign import ccall unsafe "mouseMotionPositionX" motionPositionX
               :: Ptr () -> IO CInt

foreign import ccall unsafe "mouseMotionPositionY" motionPositionY
               :: Ptr () -> IO CInt

foreign import ccall unsafe "mouseMotionMotionX" motionMotionX ::
               Ptr () -> IO CInt

foreign import ccall unsafe "mouseMotionMotionY" motionMotionY ::
               Ptr () -> IO CInt

foreign import ccall unsafe "getScrolls" cGetScrolls ::
               Window -> IO (Ptr (Ptr ()))

foreign import ccall unsafe "scrollX" scrollX :: Ptr () -> IO CInt

foreign import ccall unsafe "scrollY" scrollY :: Ptr () -> IO CInt
