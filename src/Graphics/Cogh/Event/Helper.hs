module Graphics.Cogh.Event.Helper
  ( getEvents
  , cBool
  ) where

import Foreign.C
import Foreign.Marshal.Array
import Foreign.Ptr
import Graphics.Cogh.Window.Internal

getEvents :: (Window -> IO (Ptr (Ptr ())))
          -> (Ptr () -> IO a)
          -> Window
          -> IO [a]
getEvents cGetEvents castEvent w = do
  ptrs <- peekArray0 nullPtr =<< cGetEvents w
  traverse castEvent ptrs

cBool :: CInt -> Bool
cBool = (0 /=)
