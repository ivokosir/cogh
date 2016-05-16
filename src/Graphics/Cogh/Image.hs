module Graphics.Cogh.Image
  ( newTextureFromImage
  )where

import Foreign.C
import Foreign.Ptr
import Graphics.Cogh.Render
import Graphics.Cogh.Window.Internal

newTextureFromImage :: Window -> FilePath -> IO Texture
newTextureFromImage w file = do
  cTexture <- withCString file $ cNewTextureFromImage w
  newTexture cTexture

foreign import ccall unsafe "newTextureFromImage" cNewTextureFromImage
  :: Window -> CString -> IO (Ptr ())
