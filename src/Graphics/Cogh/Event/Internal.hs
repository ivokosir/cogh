module Graphics.Cogh.Event.Internal
  ( getEvents
  , cBool
  , module Foreign.C
  , module Foreign.Ptr
  ) where

import Foreign.C
import Foreign.Ptr
import Foreign.Marshal.Array
import Graphics.Cogh.Window.Internal

getEvents
  :: (WindowPtr -> IO (Ptr (Ptr ())))
  -> (Ptr () -> IO a)
  -> WindowPtr
  -> IO [a]
getEvents cGetEvents castEvent w = do
  ptrs <- peekArray0 nullPtr =<< cGetEvents w
  traverse castEvent ptrs

cBool :: CInt -> Bool
cBool = (0 /=)
