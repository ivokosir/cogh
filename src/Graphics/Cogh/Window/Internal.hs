module Graphics.Cogh.Window.Internal
  ( WindowPtr (..)
  ) where

import Foreign.Ptr

newtype WindowPtr = WindowPtr (Ptr ())
