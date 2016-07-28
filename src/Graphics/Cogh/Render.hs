module Graphics.Cogh.Render
  ( newTexture
  , clear
  , swapBuffers
  , drawRect
  , Texture
  , drawTexture
  , textureSize
  ) where

import Foreign.C
import Foreign.Ptr
import Foreign.ForeignPtr
import Graphics.Cogh.Color
import Graphics.Cogh.Window.CWindow
import Graphics.Cogh.Matrix
import Graphics.Cogh.Vector

data Texture = Texture (ForeignPtr ()) Pixel
type TexturePtr = Ptr ()

newTexture :: Ptr () -> IO Texture
newTexture p = do
  foreignPtr <- newForeignPtr deleteTextureFunPtr p
  width <- cTextureWidth p
  height <- cTextureHeight p
  return $ Texture foreignPtr $ Point (fromIntegral width) (fromIntegral height)

textureSize :: Texture -> Pixel
textureSize (Texture _ p) = p

withCTexture :: Texture -> (Ptr () -> IO a) -> IO a
withCTexture (Texture foreignPtr _) = withForeignPtr foreignPtr

drawTexture :: WindowPtr -> Matrix -> Texture -> IO ()
drawTexture window matrix texture =
  withMatrixPtr matrix $ \ cMatrix ->
    withCTexture texture $ cDrawTexture window cMatrix

foreign import ccall unsafe "drawTexture" cDrawTexture
  :: WindowPtr
  -> Ptr Float
  -> TexturePtr
  -> IO ()

foreign import ccall unsafe "textureWidth" cTextureWidth
  :: TexturePtr -> IO CInt

foreign import ccall unsafe "textureHeight" cTextureHeight
  :: TexturePtr -> IO CInt

foreign import ccall unsafe "&deleteTexture" deleteTextureFunPtr
  :: FunPtr (TexturePtr -> IO ())


foreign import ccall unsafe "clear" clear
  :: WindowPtr -> IO ()

foreign import ccall unsafe "swapBuffers" swapBuffers
  :: WindowPtr -> IO ()

drawRect :: WindowPtr -> Matrix -> Color -> IO ()
drawRect window matrix color =
  withMatrixPtr matrix $ \ cMatrix ->
    withColorPtr color $ \ cColor ->
      cDrawRect window cMatrix cColor

foreign import ccall unsafe "drawRect" cDrawRect
  :: WindowPtr
  -> Ptr Float
  -> Ptr Float
  -> IO ()
