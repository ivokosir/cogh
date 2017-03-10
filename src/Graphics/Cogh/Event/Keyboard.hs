module Graphics.Cogh.Event.Keyboard
  ( Event(..)
  , Code(..)
  , State(..)
  , getEvents
  ) where

import Foreign.C
import Foreign.Ptr
import qualified Graphics.Cogh.Event.Helper as Helper
import Graphics.Cogh.Window.Internal
import System.IO.Unsafe

data Event = Event
  { code :: Code
  , state :: State
  } deriving (Eq, Read, Show)

newtype Code =
  Code CUInt
  deriving (Eq)

data State
  = Press Bool
  | Release
  deriving (Eq, Read, Show)

getEvents :: Window -> IO [Event]
getEvents = Helper.getEvents cGetKeys castKey

castKey :: Ptr () -> IO Event
castKey cKey = do
  cCode <- keyCode cKey
  isPress <- keyIsPress cKey
  isRepeat <- keyIsRepeat cKey
  return
    Event
    { code = Code cCode
    , state =
        if Helper.cBool isPress
          then Press (Helper.cBool isRepeat)
          else Release
    }

instance Read Code where
  readsPrec _ s = [(Code fromString, "")]
    where
      fromString = unsafePerformIO $ withCString s $ return . readCode

instance Show Code where
  show (Code cCode) = toString
    where
      toString = unsafePerformIO $ peekCString $ showCode cCode

foreign import ccall unsafe "getKeys" cGetKeys ::
               Window -> IO (Ptr (Ptr ()))

foreign import ccall unsafe "keyIsPress" keyIsPress ::
               Ptr () -> IO CInt

foreign import ccall unsafe "keyIsRepeat" keyIsRepeat ::
               Ptr () -> IO CInt

foreign import ccall unsafe "keyCode" keyCode :: Ptr () -> IO CUInt

foreign import ccall unsafe "readCode" readCode :: CString -> CUInt

foreign import ccall unsafe "showCode" showCode :: CUInt -> CString
