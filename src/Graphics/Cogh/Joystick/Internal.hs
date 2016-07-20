module Graphics.Cogh.Joystick.Internal
  ( Joystick (..)
  , Id
  , Button (..)
  , Code
  , State (..)
  , Axis (..)
  , AxisId
  , getJoysticks
  , getButtons
  , getAxii
  ) where

import Prelude hiding (id)

import Data.List
import qualified Graphics.Cogh.Button as Button
import Graphics.Cogh.Event.Internal
import Graphics.Cogh.Window.Internal

getJoysticks :: WindowPtr -> [Joystick] -> IO [Joystick]
getJoysticks w oldJoysticks = do
  newAddRemoves  <- getAddRemoves w
  newButtons     <- getButtons w
  newAxii        <- getAxii w
  let
    newJoysticks = foldl addOrRemove oldJoysticks newAddRemoves
    filledJoysticks = fmap (addButtonsAndAxii newButtons newAxii) newJoysticks
  return filledJoysticks
 where
  addOrRemove old (addId, numOfAxii, True) = Joystick
    { id = addId
    , buttons = []
    , pressedButtons = []
    , axii = fmap newEmptyAxis [0, 1..numOfAxii-1]
    } : old
  addOrRemove old (removeId, _, _) = filter (\ j -> id j /= removeId) old
  newEmptyAxis newAxisId = Axis newAxisId 0

addButtonsAndAxii :: [(Id, Button)] -> [(Id, Axis)] -> Joystick -> Joystick
addButtonsAndAxii newButtonsWithId newAxiiWithId joystick = joystick
  { buttons = thisButtons
  , pressedButtons = newPressedButtons
  , axii = newAxii
  }
 where
  isThis (newId, _) = newId == id joystick
  thisButtonsWithIds = filter isThis newButtonsWithId
  thisButtons = fmap snd thisButtonsWithIds
  oldPressedButtons = pressedButtons joystick
  newPressedButtons = Button.getPressedButtons code thisButtons oldPressedButtons
  thisAxiiWithIds = filter isThis newAxiiWithId
  thisAxii = fmap snd thisAxiiWithIds
  oldAxii = axii joystick
  newAxii = fmap changeAxis oldAxii
  changeAxis oldAxis = case mNewAxis of
    Just newAxis -> newAxis
    _ -> oldAxis
   where
    mNewAxis = find (\ newAxis -> axisId newAxis == axisId oldAxis) thisAxii

data Joystick = Joystick
  { id :: Id
  , buttons :: [Button]
  , pressedButtons :: [Code]
  , axii :: [Axis]
  } deriving (Eq, Show, Read)

type Id = Int

getAddRemoves :: WindowPtr -> IO [(Id, Int, Bool)]
getAddRemoves = getEvents cGetJoysticks castAddRemove

castAddRemove :: Ptr () -> IO (Id, Int, Bool)
castAddRemove cJoystick = do
  addRemoveId <- cId cJoystick
  numOfAxii <- cNumberOfAxii cJoystick
  isAdd <- cIsAdd cJoystick
  return
    ( fromIntegral addRemoveId
    , fromIntegral numOfAxii
    , cBool isAdd
    )


data Button = Button
  { code :: Code
  , state :: State
  } deriving (Eq, Read, Show)

instance Button.Button Button where
  isPressed (Button _ Press) = True
  isPressed _ = False

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


foreign import ccall unsafe "getJoysticks" cGetJoysticks
  :: WindowPtr -> IO (Ptr (Ptr ()))

foreign import ccall unsafe "joystickId" cId
  :: Ptr () -> IO CUInt

foreign import ccall unsafe "joystickNumberOfAxii" cNumberOfAxii
  :: Ptr () -> IO CInt

foreign import ccall unsafe "joystickIsAdd" cIsAdd
  :: Ptr () -> IO CInt


foreign import ccall unsafe "getJoystickButtons" cGetButtons
  :: WindowPtr -> IO (Ptr (Ptr ()))

foreign import ccall unsafe "joystickButtonId" buttonId
  :: Ptr () -> IO CUInt

foreign import ccall unsafe "joystickButtonIsPress" buttonIsPress
  :: Ptr () -> IO CInt

foreign import ccall unsafe "joystickButtonCode" buttonCode
  :: Ptr () -> IO CUInt


foreign import ccall unsafe "getJoystickAxii" cGetAxii
  :: WindowPtr -> IO (Ptr (Ptr ()))

foreign import ccall unsafe "joystickAxisId" cAxisId
  :: Ptr () -> IO CUInt

foreign import ccall unsafe "joystickAxisAxis" axisAxis
  :: Ptr () -> IO CUInt

foreign import ccall unsafe "joystickAxisValue" axisValue
  :: Ptr () -> IO CDouble

