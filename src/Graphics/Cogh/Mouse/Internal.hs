module Graphics.Cogh.Mouse.Internal
  ( Button (..)
  , Code (..)
  , State (..)
  , Position
  , Scroll
  , getButtons
  , getPositions
  , getScrolls
  ) where

import qualified Graphics.Cogh.Button as Button
import Graphics.Cogh.Event.Internal
import Graphics.Cogh.Window.Internal

import Prelude hiding (Left, Right)

data Button = Button
  { code :: Code
  , state ::State
  } deriving (Eq, Read, Show)

instance Button.Button Button where
  isPressed (Button _ Press) = True
  isPressed _ = False
  isSame a b = code a == code b

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


type Position = (Int, Int)

getPositions :: WindowPtr -> IO [Position]
getPositions = getEvents cGetPositions castPosition

castPosition :: Ptr () -> IO Position
castPosition cPosition = do
  x <- positionX cPosition
  y <- positionY cPosition
  return (fromIntegral x, fromIntegral y)


type Scroll = (Int, Int)

getScrolls :: WindowPtr -> IO [Scroll]
getScrolls = getEvents cGetScrolls castScroll

castScroll :: Ptr () -> IO Scroll
castScroll cScroll = do
  x <- scrollX cScroll
  y <- scrollY cScroll
  return (fromIntegral x, fromIntegral y)


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


foreign import ccall unsafe "getMousePositions" cGetPositions
  :: WindowPtr -> IO (Ptr (Ptr ()))

foreign import ccall unsafe "mousePositionX" positionX
  :: Ptr () -> IO CInt

foreign import ccall unsafe "mousePositionY" positionY
  :: Ptr () -> IO CInt


foreign import ccall unsafe "getScrolls" cGetScrolls
  :: WindowPtr -> IO (Ptr (Ptr ()))

foreign import ccall unsafe "scrollX" scrollX
  :: Ptr () -> IO CInt

foreign import ccall unsafe "scrollY" scrollY
  :: Ptr () -> IO CInt

