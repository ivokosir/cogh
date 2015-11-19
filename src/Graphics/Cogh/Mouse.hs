module Graphics.Cogh.Mouse
  ( maybeMouseButton
  , maybeMousePosition
  , maybeScroll
  )where

import Graphics.Cogh.CommonFFI

maybeMouseButton :: Window -> IO (Maybe Event)
maybeMouseButton = maybeEvent getMouseButton $ \ p -> do
  code <- mouseButtonCode p
  isLeft <- mouseButtonIsLeft p
  isMiddle <- mouseButtonIsMiddle p
  isRight <- mouseButtonIsRight p
  isPress <- mouseButtonIsPress p
  return $ MouseButton (fromIntegral code) (cBool isPress) $
    if cBool isLeft then LeftButton else
      if cBool isMiddle then MiddleButton else
        if cBool isRight then RightButton else OtherButton

maybeMousePosition :: Window -> IO (Maybe Event)
maybeMousePosition = maybeEvent getMousePosition $ \ p -> do
  x <- mousePositionX p
  y <- mousePositionY p
  return $ MousePosition (fromIntegral x) (fromIntegral y)

maybeScroll :: Window -> IO (Maybe Event)
maybeScroll = maybeEvent getScroll $ \ p -> do
  x <- scrollX p
  y <- scrollY p
  return $ Scroll (fromIntegral x) (fromIntegral y)


foreign import ccall unsafe "getMouseButton" getMouseButton
  :: Window -> IO (Ptr ())

foreign import ccall unsafe "mouseButtonIsLeft" mouseButtonIsLeft
  :: Ptr() -> IO (CInt)

foreign import ccall unsafe "mouseButtonIsMiddle" mouseButtonIsMiddle
  :: Ptr() -> IO (CInt)

foreign import ccall unsafe "mouseButtonIsRight" mouseButtonIsRight
  :: Ptr() -> IO (CInt)

foreign import ccall unsafe "mouseButtonIsPress" mouseButtonIsPress
  :: Ptr() -> IO (CInt)

foreign import ccall unsafe "mouseButtonCode" mouseButtonCode
  :: Ptr() -> IO (CUInt)


foreign import ccall unsafe "getMousePosition" getMousePosition
  :: Window -> IO (Ptr ())

foreign import ccall unsafe "mousePositionX" mousePositionX
  :: Ptr () -> IO (CInt)

foreign import ccall unsafe "mousePositionY" mousePositionY
  :: Ptr () -> IO (CInt)


foreign import ccall unsafe "getScroll" getScroll
  :: Window -> IO (Ptr ())

foreign import ccall unsafe "scrollX" scrollX
  :: Ptr () -> IO (CInt)

foreign import ccall unsafe "scrollY" scrollY
  :: Ptr () -> IO (CInt)
