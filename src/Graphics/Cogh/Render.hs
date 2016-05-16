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
import Graphics.Cogh.Window.Internal

data Texture = Texture (ForeignPtr ()) Int Int

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

foreign import ccall unsafe "clear" clear
  :: Window -> IO ()

foreign import ccall unsafe "swapBuffers" swapBuffers
  :: Window -> IO ()

foreign import ccall unsafe "drawRect" drawRect
  :: Window
  -> Ptr Float
  -> Ptr Float
  -> IO ()

foreign import ccall unsafe "drawTexture" cDrawTexture
  :: Window
  -> Ptr Float
  -> Ptr ()
  -> IO ()
drawTexture
  :: Window
  -> Ptr Float
  -> Texture
  -> IO ()
drawTexture w matrix texture =
  withCTexture texture $ cDrawTexture w matrix

foreign import ccall unsafe "textureWidth" cTextureWidth
  :: Ptr () -> IO CInt

foreign import ccall unsafe "textureHeight" cTextureHeight
  :: Ptr () -> IO CInt

foreign import ccall unsafe "&deleteTexture" deleteTextureFunPtr
  :: FunPtr (Ptr () -> IO ())
