module Graphics.Cogh.Joystick.Internal
  ( Button (..)
  , Id
  , Code
  , State (..)
  , Axis (..)
  , AxisId
  , getButtons
  , getAxii
  ) where

import Graphics.Cogh.CommonFFI

data Button = Button Id Code State deriving (Eq, Read, Show)

type Id = Int

type Code = Int

data State = Press | Release deriving (Eq, Read, Show)

getButtons :: Window -> IO [Button]
getButtons = getEvents cGetButtons castButton

castButton :: Ptr () -> IO Button
castButton cButton = do
  joystickId <- buttonId cButton
  code <- buttonCode cButton
  isPress <- buttonIsPress cButton
  let state = if cBool isPress then Press else Release
  return $ Button (fromIntegral joystickId)
    (fromIntegral code) state


data Axis = Axis Id AxisId Double deriving (Eq, Read, Show)

type AxisId = Int

getAxii :: Window -> IO [Axis]
getAxii = getEvents cGetAxii castAxis

castAxis :: Ptr () -> IO Axis
castAxis cAxis = do
  joystickId <- axisId cAxis
  axis <- axisAxis cAxis
  value <- axisValue cAxis
  return $ Axis (fromIntegral joystickId)
    (fromIntegral axis) (realToFrac value)


foreign import ccall unsafe "getJoystickButtons" cGetButtons
  :: Window -> IO (Ptr (Ptr ()))

foreign import ccall unsafe "joystickButtonId" buttonId
  :: Ptr () -> IO (CInt)

foreign import ccall unsafe "joystickButtonIsPress" buttonIsPress
  :: Ptr () -> IO (CInt)

foreign import ccall unsafe "joystickButtonCode" buttonCode
  :: Ptr () -> IO (CUInt)


foreign import ccall unsafe "getJoystickAxii" cGetAxii
  :: Window -> IO (Ptr (Ptr ()))

foreign import ccall unsafe "joystickAxisId" axisId
  :: Ptr () -> IO (CInt)

foreign import ccall unsafe "joystickAxisAxis" axisAxis
  :: Ptr () -> IO (CUInt)

foreign import ccall unsafe "joystickAxisValue" axisValue
  :: Ptr () -> IO (CDouble)

