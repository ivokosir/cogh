module Graphics.Cogh.Event.Mouse
  ( Event(..)
  , Button(..)
  , Code(..)
  , State(..)
  , Motion(..)
  , Position
  , Scroll
  , getEvents
  ) where

import Foreign.C
import Foreign.Ptr
import qualified Graphics.Cogh.Event.Helper as Helper
import Graphics.Cogh.Vector
import Graphics.Cogh.Window.Internal

import Prelude hiding (Left, Right)

data Event
  = EventButton Button
  | EventMotion Motion
  | EventScroll Scroll

data Button = Button
  { code :: Code
  , state :: State
  } deriving (Eq, Read, Show)

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

getEvents :: Window -> IO [Event]
getEvents w = do
  buttons <- getButtons w
  motions <- getMotions w
  scrolls <- getScrolls w
  return $
    concat
      [ fmap EventButton buttons
      , fmap EventMotion motions
      , fmap EventScroll scrolls
      ]

getButtons :: Window -> IO [Button]
getButtons = Helper.getEvents cGetButtons castButton

castButton :: Ptr () -> IO Button
castButton cButton = do
  ccode <- buttonCode cButton
  isLeft <- buttonIsLeft cButton
  isMiddle <- buttonIsMiddle cButton
  isRight <- buttonIsRight cButton
  isPress <- buttonIsPress cButton
  let code'
        | Helper.cBool isLeft = Left
        | Helper.cBool isMiddle = Middle
        | Helper.cBool isRight = Right
        | otherwise = Other (fromIntegral ccode)
  return
    Button
    { code = code'
    , state =
        if Helper.cBool isPress
          then Press
          else Release
    }

data Motion = Motion
  { position :: Position
  , motion :: Pixel
  } deriving (Eq, Show, Read)

type Position = Pixel

getMotions :: Window -> IO [Motion]
getMotions = Helper.getEvents cGetMotions castMotion

castMotion :: Ptr () -> IO Motion
castMotion cMotion = do
  positionX <- motionPositionX cMotion
  positionY <- motionPositionY cMotion
  cMotionX <- motionMotionX cMotion
  cMotionY <- motionMotionY cMotion
  return
    Motion
    { position = Point (fromIntegral positionX) (fromIntegral positionY)
    , motion = Point (fromIntegral cMotionX) (fromIntegral cMotionY)
    }

type Scroll = Pixel

getScrolls :: Window -> IO [Scroll]
getScrolls = Helper.getEvents cGetScrolls castScroll

castScroll :: Ptr () -> IO Scroll
castScroll cScroll = do
  x <- scrollX cScroll
  y <- scrollY cScroll
  return $ Point (fromIntegral x) (fromIntegral y)

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
