module Graphics.Cogh.Image
  ( newTextureFromImage
  )where

import Graphics.Cogh.CommonFFI


foreign import ccall unsafe "newTextureFromImage" newTextureFromImage'
  :: Window -> CString -> IO Texture
newTextureFromImage :: Window -> String -> IO Texture
newTextureFromImage w file = withCString file $ newTextureFromImage' w
