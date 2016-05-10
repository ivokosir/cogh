module Graphics.Cogh.Image
  ( newTextureFromImage
  )where

import Graphics.Cogh.CommonFFI


foreign import ccall unsafe "newTextureFromImage" cNewTextureFromImage
  :: Window -> CString -> IO Texture
newTextureFromImage :: Window -> FilePath -> IO Texture
newTextureFromImage w file = withCString file $ cNewTextureFromImage w
