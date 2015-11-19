module Graphics.Cogh.Joystick
  ( maybeJoystickButton
  , maybeJoystickAxis
  ) where

import Graphics.Cogh.CommonFFI

maybeJoystickButton :: Window -> IO (Maybe Event)
maybeJoystickButton = maybeEvent getJoystickButton $ \ p -> do
  id' <- joystickButtonId p
  code <- joystickButtonCode p
  isPress <- joystickButtonIsPress p
  return $ JoystickButton (fromIntegral id')
    (fromIntegral code) (cBool isPress)

maybeJoystickAxis :: Window -> IO (Maybe Event)
maybeJoystickAxis = maybeEvent getJoystickAxis $ \ p -> do
  id' <- joystickAxisId p
  axis <- joystickAxisAxis p
  value <- joystickAxisValue p
  return $ JoystickAxis (fromIntegral id')
    (fromIntegral axis) (realToFrac value)


foreign import ccall unsafe "getJoystickButton" getJoystickButton
  :: Window -> IO (Ptr ())

foreign import ccall unsafe "joystickButtonId" joystickButtonId
  :: Ptr () -> IO (CInt)

foreign import ccall unsafe "joystickButtonIsPress" joystickButtonIsPress
  :: Ptr () -> IO (CInt)

foreign import ccall unsafe "joystickButtonCode" joystickButtonCode
  :: Ptr () -> IO (CUInt)


foreign import ccall unsafe "getJoystickAxis" getJoystickAxis
  :: Window -> IO (Ptr ())

foreign import ccall unsafe "joystickAxisId" joystickAxisId
  :: Ptr () -> IO (CInt)

foreign import ccall unsafe "joystickAxisAxis" joystickAxisAxis
  :: Ptr () -> IO (CUInt)

foreign import ccall unsafe "joystickAxisValue" joystickAxisValue
  :: Ptr () -> IO (CDouble)
