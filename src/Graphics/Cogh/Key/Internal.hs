module Graphics.Cogh.Key.Internal
  ( Key (..)
  , Code (..)
  , State (..)
  , getKeys
  )where

import Graphics.Cogh.CommonFFI

data Key = Key Code State deriving (Eq)

newtype Code = Code CUInt deriving (Eq)

data State = Press | Release Bool deriving (Eq, Read, Show)

getKeys :: Window -> IO [Key]
getKeys = getEvents cGetKeys castKey

castKey :: Ptr () -> IO Key
castKey cKey = do
  code <- keyCode cKey
  isPress <- keyIsPress cKey
  isRepeat <- keyIsRepeat cKey
  let state = if cBool isPress then Press else Release $ cBool isRepeat
  return $ Key (Code code) state

foreign import ccall unsafe "getKeys" cGetKeys
  :: Window -> IO (Ptr (Ptr ()))

foreign import ccall unsafe "keyIsPress" keyIsPress
  :: Ptr () -> IO CInt

foreign import ccall unsafe "keyIsRepeat" keyIsRepeat
  :: Ptr () -> IO CInt

foreign import ccall unsafe "keyCode" keyCode
  :: Ptr () -> IO CUInt
