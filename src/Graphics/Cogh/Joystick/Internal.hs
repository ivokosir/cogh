module Graphics.Cogh.Joystick.Internal
  ( Joystick (..)
  , Id
  , Button (..)
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

data Joystick = Joystick
  { id :: Id
  , buttons :: [Button]
  , axii :: [Axis]
  }

type Id = Int

data Button = Button
  { code :: Code
  , state :: State
  } deriving (Eq, Read, Show)

type Code = Int

data State = Press | Release deriving (Eq, Read, Show)

getButtons :: WindowPtr -> IO [(Id, Button)]
getButtons = getEvents cGetButtons castButton

castButton :: Ptr () -> IO (Id, Button)
castButton cButton = do
  joystickId <- buttonId cButton
  cCode <- buttonCode cButton
  isPress <- buttonIsPress cButton
  return
    ( fromIntegral joystickId
    , Button
      { code = fromIntegral cCode
      , state = if cBool isPress then Press else Release
      }
    )


data Axis = Axis
  { axisId :: AxisId
  , value :: Double
  } deriving (Eq, Read, Show)

type AxisId = Int

getAxii :: WindowPtr -> IO [(Id, Axis)]
getAxii = getEvents cGetAxii castAxis

castAxis :: Ptr () -> IO (Id, Axis)
castAxis cAxis = do
  joystickId <- cAxisId cAxis
  axis <- axisAxis cAxis
  cValue <- axisValue cAxis
  return
    ( fromIntegral joystickId
    , Axis
      { axisId = fromIntegral axis
      , value = realToFrac cValue
      }
    )


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

foreign import ccall unsafe "joystickAxisId" cAxisId
  :: Ptr () -> IO CInt

foreign import ccall unsafe "joystickAxisAxis" axisAxis
  :: Ptr () -> IO CUInt

foreign import ccall unsafe "joystickAxisValue" axisValue
  :: Ptr () -> IO CDouble

