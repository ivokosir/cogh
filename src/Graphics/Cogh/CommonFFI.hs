module Graphics.Cogh.CommonFFI
  ( Window (..)
  , Texture (..)
  , Event (..)
  , KeyCode (..)
  , MouseButton (..)
  , maybeEvent
  , cBool
  , module Foreign.C
  , module Foreign.Ptr
  ) where

import Foreign.C
import Foreign.Ptr


newtype Texture = Texture (Ptr ())

newtype Window = Window (Ptr ())

data Event
  = Quit
  | Resize Int Int
  | Key KeyCode Bool Bool
  | MouseButton Int Bool MouseButton
  | MousePosition Int Int
  | Scroll Int Int
  | JoystickButton Int Int Bool
  | JoystickAxis Int Int Double

newtype KeyCode = KeyCode CUInt

data MouseButton = LeftButton | MiddleButton | RightButton | OtherButton

maybeEvent
  :: (Window -> IO (Ptr ())) -- ^C function, can return NULL
  -> (Ptr () -> IO (Event)) -- ^Funcion that converts C Ptr to Event.Event
  -> Window
  -> IO (Maybe Event) -- ^Returns Event.Event safely
maybeEvent get f w = do
  p <- get w
  if p /= nullPtr
    then f p >>= return . Just
    else return Nothing

cBool :: CInt -> Bool
cBool = (0 /=)
