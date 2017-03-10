module Graphics.Cogh.Image
  ( newTextureFromImage
  ) where

import Foreign.C
import Foreign.Ptr
import Graphics.Cogh.Render
import Graphics.Cogh.Window.Internal

type ImagePtr = Ptr ()

newTextureFromImage :: Window -> FilePath -> IO Texture
newTextureFromImage w file = do
  cTexture <- withCString file $ \cString -> cNewTextureFromImage w cString
  newTexture cTexture

foreign import ccall unsafe "newTextureFromImage"
               cNewTextureFromImage :: Window -> CString -> IO ImagePtr
