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

import Graphics.Cogh.Event.Internal
import Graphics.Cogh.Window.Internal

import Prelude hiding (Left, Right)

data Button = Button Code State deriving (Eq, Read, Show)

data Code = Left | Middle | Right | Other Int deriving (Eq, Read, Show)

data State = Press | Release deriving (Eq, Read, Show)

getButtons :: Window -> IO [Button]
getButtons = getEvents cGetButtons castButton

castButton :: Ptr () -> IO Button
castButton cButton = do
  ccode <- buttonCode cButton
  isLeft <- buttonIsLeft cButton
  isMiddle <- buttonIsMiddle cButton
  isRight <- buttonIsRight cButton
  isPress <- buttonIsPress cButton
  let
    code
      | cBool isLeft = Left
      | cBool isMiddle = Middle
      | cBool isRight = Right
      | otherwise = Other (fromIntegral ccode)
    state =
      if cBool isPress then Press else Release
  return $ Button code state


type Position = (Int, Int)

getPositions :: Window -> IO [Position]
getPositions = getEvents cGetPositions castPosition

castPosition :: Ptr () -> IO Position
castPosition cPosition = do
  x <- positionX cPosition
  y <- positionY cPosition
  return (fromIntegral x, fromIntegral y)


type Scroll = (Int, Int)

getScrolls :: Window -> IO [Scroll]
getScrolls = getEvents cGetScrolls castScroll

castScroll :: Ptr () -> IO Scroll
castScroll cScroll = do
  x <- scrollX cScroll
  y <- scrollY cScroll
  return (fromIntegral x, fromIntegral y)


foreign import ccall unsafe "getMouseButtons" cGetButtons
  :: Window -> IO (Ptr (Ptr ()))

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
  :: Window -> IO (Ptr (Ptr ()))

foreign import ccall unsafe "mousePositionX" positionX
  :: Ptr () -> IO CInt

foreign import ccall unsafe "mousePositionY" positionY
  :: Ptr () -> IO CInt


foreign import ccall unsafe "getScrolls" cGetScrolls
  :: Window -> IO (Ptr (Ptr ()))

foreign import ccall unsafe "scrollX" scrollX
  :: Ptr () -> IO CInt

foreign import ccall unsafe "scrollY" scrollY
  :: Ptr () -> IO CInt

