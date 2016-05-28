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

import Prelude hiding (id)

import Graphics.Cogh.Event.Internal
import Graphics.Cogh.Window.Internal

data Button = Button
  { id :: Id
  , code :: Code
  , state :: State
  } deriving (Eq, Read, Show)

type Id = Int

type Code = Int

data State = Press | Release deriving (Eq, Read, Show)

getButtons :: WindowPtr -> IO [Button]
getButtons = getEvents cGetButtons castButton

castButton :: Ptr () -> IO Button
castButton cButton = do
  joystickId <- buttonId cButton
  cCode <- buttonCode cButton
  isPress <- buttonIsPress cButton
  return Button
    { id = fromIntegral joystickId
    , code = fromIntegral cCode
    , state = if cBool isPress then Press else Release
    }


data Axis = Axis Id AxisId Double deriving (Eq, Read, Show)

type AxisId = Int

getAxii :: WindowPtr -> IO [Axis]
getAxii = getEvents cGetAxii castAxis

castAxis :: Ptr () -> IO Axis
castAxis cAxis = do
  joystickId <- axisId cAxis
  axis <- axisAxis cAxis
  value <- axisValue cAxis
  return $ Axis (fromIntegral joystickId)
    (fromIntegral axis) (realToFrac value)


foreign import ccall unsafe "getJoystickButtons" cGetButtons
  :: WindowPtr -> IO (Ptr (Ptr ()))

foreign import ccall unsafe "joystickButtonId" buttonId
  :: Ptr () -> IO CInt

foreign import ccall unsafe "joystickButtonIsPress" buttonIsPress
  :: Ptr () -> IO CInt

foreign import ccall unsafe "joystickButtonCode" buttonCode
  :: Ptr () -> IO CUInt


foreign import ccall unsafe "getJoystickAxii" cGetAxii
  :: WindowPtr -> IO (Ptr (Ptr ()))

foreign import ccall unsafe "joystickAxisId" axisId
  :: Ptr () -> IO CInt

foreign import ccall unsafe "joystickAxisAxis" axisAxis
  :: Ptr () -> IO CUInt

foreign import ccall unsafe "joystickAxisValue" axisValue
  :: Ptr () -> IO CDouble

