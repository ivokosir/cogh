module Graphics.Cogh.Event.Keyboard
  ( Key
  , code
  , state
  , readCode
  , showCode
  , State(..)
  , getKeys
  ) where

import Foreign.C
import Foreign.Ptr
import Graphics.Cogh.Event.Helper
import Graphics.Cogh.Window.Internal
import System.IO.Unsafe

data Key =
  Key Int
      State
  deriving (Eq, Read, Show)

code :: Key -> Int
code (Key c _) = c

state :: Key -> State
state (Key _ s) = s

readCode :: String -> Int
readCode s = fromIntegral $ unsafePerformIO $ withCString s $ return . cReadCode

showCode :: Int -> String
showCode = unsafePerformIO . peekCString . cShowCode . fromIntegral

data State
  = Press Bool
  | Release
  deriving (Eq, Read, Show)

getKeys :: Window -> IO [Key]
getKeys = getEvents cGetKeys castKey

castKey :: Ptr () -> IO Key
castKey cKey = do
  cCode <- keyCode cKey
  isPress <- keyIsPress cKey
  isRepeat <- keyIsRepeat cKey
  return $
    Key
      (fromIntegral cCode)
      (if cBool isPress
         then Press (cBool isRepeat)
         else Release)

foreign import ccall unsafe "getKeys" cGetKeys ::
               Window -> IO (Ptr (Ptr ()))

foreign import ccall unsafe "keyIsPress" keyIsPress ::
               Ptr () -> IO CInt

foreign import ccall unsafe "keyIsRepeat" keyIsRepeat ::
               Ptr () -> IO CInt

foreign import ccall unsafe "keyCode" keyCode :: Ptr () -> IO CUInt

foreign import ccall unsafe "readCode" cReadCode ::
               CString -> CUInt

foreign import ccall unsafe "showCode" cShowCode ::
               CUInt -> CString
