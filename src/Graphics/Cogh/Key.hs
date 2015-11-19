module Graphics.Cogh.Key
  ( maybeKey
  )where

import Graphics.Cogh.CommonFFI

maybeKey :: Window -> IO (Maybe Event)
maybeKey = maybeEvent getKey $ \ p -> do
  code <- keyCode p
  isPress <- keyIsPress p
  isRepeat <- keyIsRepeat p
  return $ Key (KeyCode code) (cBool isPress) (cBool isRepeat)


foreign import ccall unsafe "getKey" getKey
  :: Window -> IO (Ptr ())

foreign import ccall unsafe "keyIsPress" keyIsPress
  :: Ptr () -> IO CInt

foreign import ccall unsafe "keyIsRepeat" keyIsRepeat
  :: Ptr () -> IO CInt

foreign import ccall unsafe "keyCode" keyCode
  :: Ptr () -> IO CUInt
