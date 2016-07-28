module Graphics.Cogh.Key.Internal
  ( Key (..)
  , Code (..)
  , State (..)
  , getKeys
  ) where

import System.IO.Unsafe
import Graphics.Cogh.Button
import Graphics.Cogh.Event.Internal
import Graphics.Cogh.Window.CWindow

data Key = Key
  { code :: Code
  , state :: State
  } deriving (Eq, Read, Show)

instance Button Key where
  isPressed (Key _ (Press _)) = True
  isPressed _ = False

newtype Code = Code CUInt deriving (Eq)

data State = Press Bool | Release deriving (Eq, Read, Show)

getKeys :: WindowPtr -> IO [Key]
getKeys = getEvents cGetKeys castKey

castKey :: Ptr () -> IO Key
castKey cKey = do
  cCode <- keyCode cKey
  isPress <- keyIsPress cKey
  isRepeat <- keyIsRepeat cKey
  return Key
    { code = Code cCode
    , state = if cBool isPress then Press (cBool isRepeat) else Release
    }

instance Read Code where
  readsPrec _ s = [(Code fromString, "")]
   where
    fromString = unsafePerformIO $ withCString s $ return . readCode

instance Show Code where
  show (Code cCode) = toString
   where
    toString = unsafePerformIO $ peekCString $ showCode cCode

foreign import ccall unsafe "getKeys" cGetKeys
  :: WindowPtr -> IO (Ptr (Ptr ()))

foreign import ccall unsafe "keyIsPress" keyIsPress
  :: Ptr () -> IO CInt

foreign import ccall unsafe "keyIsRepeat" keyIsRepeat
  :: Ptr () -> IO CInt

foreign import ccall unsafe "keyCode" keyCode
  :: Ptr () -> IO CUInt

foreign import ccall unsafe "readCode" readCode
  :: CString -> CUInt

foreign import ccall unsafe "showCode" showCode
  :: CUInt -> CString
