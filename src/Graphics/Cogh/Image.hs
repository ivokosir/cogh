module Graphics.Cogh.Image
  ( newTextureFromImage
  ) where

import Foreign.C
import Foreign.Ptr
import Graphics.Cogh.Render
import Graphics.Cogh.Window.Internal

type ImagePtr = Ptr ()

newTextureFromImage :: Window -> FilePath -> IO Texture
newTextureFromImage window file = do
  cTexture <-
    withCWindow window $ \ cWindow ->
      withCString file $ \ cString ->
        cNewTextureFromImage cWindow cString
  newTexture cTexture

foreign import ccall unsafe "newTextureFromImage" cNewTextureFromImage
  :: WindowPtr -> CString -> IO ImagePtr
