module Graphics.Cogh.Window.CWindow
  ( WindowPtr (..)
  )where

import Foreign.Ptr

newtype WindowPtr = WindowPtr (Ptr ())
