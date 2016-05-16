module Graphics.Cogh.Key.Internal
  ( Key (..)
  , Code (..)
  , State (..)
  , getKeys
  ) where

import System.IO.Unsafe
import Graphics.Cogh.Event.Internal
import Graphics.Cogh.Window.Internal

data Key = Key Code State deriving (Eq, Read, Show)

newtype Code = Code CUInt deriving (Eq)

data State = Press Bool | Release deriving (Eq, Read, Show)

getKeys :: Window -> IO [Key]
getKeys = getEvents cGetKeys castKey

castKey :: Ptr () -> IO Key
castKey cKey = do
  code <- keyCode cKey
  isPress <- keyIsPress cKey
  isRepeat <- keyIsRepeat cKey
  let state = if cBool isPress then Press (cBool isRepeat) else Release
  return $ Key (Code code) state

instance Read Code where
  readsPrec _ s = [(Code fromString, "")]
   where
    fromString = unsafePerformIO $ withCString s $ return . readCode

instance Show Code where
  show (Code code) = toString
   where
    toString = unsafePerformIO $ peekCString $ showCode code

foreign import ccall unsafe "getKeys" cGetKeys
  :: Window -> IO (Ptr (Ptr ()))

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
