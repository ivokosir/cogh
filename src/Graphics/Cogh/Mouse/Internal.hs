module Graphics.Cogh.Mouse.Internal
  ( Button (..)
  , Code (..)
  , State (..)
  , Motion (..)
  , Position
  , Scroll
  , getButtons
  , getMotions
  , getScrolls
  ) where

import qualified Graphics.Cogh.Button as Button
import Graphics.Cogh.Event.Internal
import Graphics.Cogh.Vector
import Graphics.Cogh.Window.Internal

import Prelude hiding (Left, Right)

data Button = Button
  { code :: Code
  , state ::State
  } deriving (Eq, Read, Show)

instance Button.Button Button where
  isPressed (Button _ Press) = True
  isPressed _ = False

data Code = Left | Middle | Right | Other Int deriving (Eq, Read, Show)

data State = Press | Release deriving (Eq, Read, Show)

getButtons :: WindowPtr -> IO [Button]
getButtons = getEvents cGetButtons castButton

castButton :: Ptr () -> IO Button
castButton cButton = do
  ccode <- buttonCode cButton
  isLeft <- buttonIsLeft cButton
  isMiddle <- buttonIsMiddle cButton
  isRight <- buttonIsRight cButton
  isPress <- buttonIsPress cButton
  let
    code'
      | cBool isLeft = Left
      | cBool isMiddle = Middle
      | cBool isRight = Right
      | otherwise = Other (fromIntegral ccode)
  return Button
    { code = code'
    , state = if cBool isPress then Press else Release
    }


data Motion = Motion
  { position :: Position
  , motion :: Pixel
  } deriving (Eq, Show, Read)

type Position = Pixel

getMotions :: WindowPtr -> IO [Motion]
getMotions = getEvents cGetMotions castMotion

castMotion :: Ptr () -> IO Motion
castMotion cMotion = do
  positionX <- motionPositionX cMotion
  positionY <- motionPositionY cMotion
  cMotionX <- motionMotionX cMotion
  cMotionY <- motionMotionY cMotion
  return Motion
    { position = Point (fromIntegral positionX) (fromIntegral positionY)
    , motion = Point (fromIntegral cMotionX) (fromIntegral cMotionY)
    }


type Scroll = Pixel

getScrolls :: WindowPtr -> IO [Scroll]
getScrolls = getEvents cGetScrolls castScroll

castScroll :: Ptr () -> IO Scroll
castScroll cScroll = do
  x <- scrollX cScroll
  y <- scrollY cScroll
  return $ Point (fromIntegral x) (fromIntegral y)


foreign import ccall unsafe "getMouseButtons" cGetButtons
  :: WindowPtr -> IO (Ptr (Ptr ()))

foreign import ccall unsafe "mouseButtonIsLeft" buttonIsLeft
  :: Ptr() -> IO CInt

foreign import ccall unsafe "mouseButtonIsMiddle" buttonIsMiddle
  :: Ptr() -> IO CInt

foreign import ccall unsafe "mouseButtonIsRight" buttonIsRight
  :: Ptr() -> IO CInt

foreign import ccall unsafe "mouseButtonIsPress" buttonIsPress
  :: Ptr() -> IO CInt

foreign import ccall unsafe "mouseButtonCode" buttonCode
  :: Ptr() -> IO CUInt


foreign import ccall unsafe "getMouseMotions" cGetMotions
  :: WindowPtr -> IO (Ptr (Ptr ()))

foreign import ccall unsafe "mouseMotionPositionX" motionPositionX
  :: Ptr () -> IO CInt

foreign import ccall unsafe "mouseMotionPositionY" motionPositionY
  :: Ptr () -> IO CInt

foreign import ccall unsafe "mouseMotionMotionX" motionMotionX
  :: Ptr () -> IO CInt

foreign import ccall unsafe "mouseMotionMotionY" motionMotionY
  :: Ptr () -> IO CInt


foreign import ccall unsafe "getScrolls" cGetScrolls
  :: WindowPtr -> IO (Ptr (Ptr ()))

foreign import ccall unsafe "scrollX" scrollX
  :: Ptr () -> IO CInt

foreign import ccall unsafe "scrollY" scrollY
  :: Ptr () -> IO CInt

