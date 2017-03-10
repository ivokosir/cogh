module Graphics.Cogh.Window.Internal
  ( Window(..)
  ) where

import Foreign.Ptr

newtype Window =
  Window (Ptr ())
