module Graphics.Cogh.Render
  ( newTexture
  , clear
  , swapBuffers
  , drawRect
  , Texture
  , drawTexture
  , textureWidth
  , textureHeight
  ) where

import Foreign.C
import Foreign.Ptr
import Foreign.ForeignPtr
import Graphics.Cogh.Event
import Graphics.Cogh.Matrix
import Graphics.Cogh.Color

data Texture = Texture (ForeignPtr ()) Int Int
type TexturePtr = Ptr ()

newTexture :: Ptr () -> IO Texture
newTexture p = do
  foreignPtr <- newForeignPtr deleteTextureFunPtr p
  width <- cTextureWidth p
  height <- cTextureHeight p
  return $ Texture foreignPtr (fromIntegral width) (fromIntegral height)

textureWidth :: Texture -> Int
textureWidth (Texture _ w _) = w

textureHeight :: Texture -> Int
textureHeight (Texture _ _ h) = h

withCTexture :: Texture -> (Ptr () -> IO a) -> IO a
withCTexture (Texture foreignPtr _ _) = withForeignPtr foreignPtr

drawTexture :: Window -> Matrix -> Texture -> IO ()
drawTexture w matrix texture =
  withCWindow w $ \ cWindow ->
    withMatrixPtr matrix $ \ cMatrix ->
      withCTexture texture $ cDrawTexture cWindow cMatrix

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


clear :: Window -> IO ()
clear window = withCWindow window cClear

foreign import ccall unsafe "clear" cClear
  :: WindowPtr -> IO ()

swapBuffers :: Window -> IO ()
swapBuffers window = withCWindow window cSwapBuffers

foreign import ccall unsafe "swapBuffers" cSwapBuffers
  :: WindowPtr -> IO ()


drawRect :: Window -> Matrix -> Color -> IO ()
drawRect window matrix color =
  withCWindow window $ \ cWindow ->
    withMatrixPtr matrix $ \ cMatrix ->
      withColorPtr color $ \ cColor ->
        cDrawRect cWindow cMatrix cColor

foreign import ccall unsafe "drawRect" cDrawRect
  :: WindowPtr
  -> Ptr Float
  -> Ptr Float
  -> IO ()
