module Graphics.Cogh.CommonFFI
  ( Window (..)
  , Texture (..)
  , getEvents
  , cBool
  , module Foreign.C
  , module Foreign.Ptr
  ) where

import Foreign.C
import Foreign.Ptr
import Foreign.Marshal.Array

newtype Window = Window (Ptr ())

newtype Texture = Texture (Ptr ())

getEvents
  :: (Window -> IO (Ptr (Ptr ())))
  -> (Ptr () -> IO a)
  -> Window
  -> IO [a]
getEvents cGetEvents castEvent w = do
  ptrs <- peekArray0 nullPtr =<< cGetEvents w
  traverse castEvent ptrs

cBool :: CInt -> Bool
cBool = (0 /=)
