module Graphics.Cogh.Render
  ( clear
  , swapBuffers
  , Texture
  , newTexture
  , drawTexture
  , textureSize
  , withColorPtr
  , drawRect
  ) where

import Foreign.C
import Foreign.ForeignPtr
import Foreign.Marshal.Array
import Foreign.Ptr
import Graphics.Cogh.Color
import Graphics.Cogh.Matrix
import Graphics.Cogh.Vector
import Graphics.Cogh.Window.Internal

foreign import ccall unsafe "clear" clear :: Window -> IO ()

foreign import ccall unsafe "swapBuffers" swapBuffers ::
               Window -> IO ()

data Texture =
  Texture (ForeignPtr ())
          Pixel

type TexturePtr = Ptr ()

newTexture :: Ptr () -> IO Texture
newTexture p = do
  foreignPtr <- newForeignPtr deleteTextureFunPtr p
  width <- cTextureWidth p
  height <- cTextureHeight p
  return $ Texture foreignPtr $ pixel (fromIntegral width) (fromIntegral height)

textureSize :: Texture -> Pixel
textureSize (Texture _ p) = p

withCTexture :: Texture -> (Ptr () -> IO a) -> IO a
withCTexture (Texture foreignPtr _) = withForeignPtr foreignPtr

withMatrixPtr :: Matrix -> (Ptr Float -> IO a) -> IO a
withMatrixPtr (Matrix a d g b e h) = withArray [a, b, 0, d, e, 0, g, h, 1]

drawTexture :: Window -> Matrix -> Texture -> IO ()
drawTexture window matrix texture =
  withMatrixPtr matrix $ \cMatrix ->
    withCTexture texture $ cDrawTexture window cMatrix

foreign import ccall unsafe "drawTexture" cDrawTexture ::
               Window -> Ptr Float -> TexturePtr -> IO ()

foreign import ccall unsafe "textureWidth" cTextureWidth ::
               TexturePtr -> IO CInt

foreign import ccall unsafe "textureHeight" cTextureHeight ::
               TexturePtr -> IO CInt

foreign import ccall unsafe "&deleteTexture" deleteTextureFunPtr ::
               FunPtr (TexturePtr -> IO ())

withColorPtr :: Color -> (Ptr Float -> IO a) -> IO a
withColorPtr (Color r g b a) = withArray [r, g, b, a]

drawRect :: Window -> Matrix -> Color -> IO ()
drawRect window matrix c =
  withMatrixPtr matrix $ \cMatrix ->
    withColorPtr c $ \cColor -> cDrawRect window cMatrix cColor

foreign import ccall unsafe "drawRect" cDrawRect ::
               Window -> Ptr Float -> Ptr Float -> IO ()
