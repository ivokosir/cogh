module Graphics.Cogh.Render
  ( clear
  , swapBuffers
  , drawRect
  , Texture
  , drawTexture
  , deleteTexture
  , textureWidth
  , textureHeight
  ) where

import Graphics.Cogh.CommonFFI

foreign import ccall unsafe "clear" clear
  :: Window -> IO ()

foreign import ccall unsafe "swapBuffers" swapBuffers
  :: Window -> IO ()

foreign import ccall unsafe "drawRect" drawRect
  :: Window
  -> Ptr Float
  -> Ptr Float
  -> IO ()

foreign import ccall unsafe "drawTexture" drawTexture
  :: Window
  -> Ptr Float
  -> Texture
  -> IO ()

foreign import ccall unsafe "textureWidth" textureWidth'
  :: Texture -> CInt
textureWidth :: Texture -> Int
textureWidth = fromIntegral . textureWidth'

foreign import ccall unsafe "textureHeight" textureHeight'
  :: Texture -> CInt
textureHeight :: Texture -> Int
textureHeight = fromIntegral . textureHeight'

foreign import ccall unsafe "deleteTexture" deleteTexture
  :: Texture -> IO ()
