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
import Data.Word

foreign import ccall unsafe "clear" clear
  :: Window -> IO ()

foreign import ccall unsafe "swapBuffers" swapBuffers
  :: Window -> IO ()

foreign import ccall unsafe "drawRect" drawRect'
  :: Window -> CInt -> CInt -> CInt -> CInt -> CUInt -> IO ()
drawRect :: Window -> Int -> Int -> Int -> Int -> Word32 -> IO ()
drawRect window x y w h color =
  drawRect' window
    (fromIntegral x) (fromIntegral y)
    (fromIntegral w) (fromIntegral h)
    (fromIntegral color)

foreign import ccall unsafe "drawTexture" drawTexture'
  :: Window -> Texture
  -> CInt -> CInt -> CInt -> CInt -- destination rectangle
  -> CDouble -> CInt -> CInt -- rotation and rotation center
  -> IO ()
drawTexture
  :: Window -> Texture
  -> Int -> Int -> Int -> Int
  -> Double -> Int -> Int
  -> IO ()
drawTexture window t x y w h angle cx cy =
  drawTexture' window t
    (fromIntegral x) (fromIntegral y)
    (fromIntegral w) (fromIntegral h)
    (CDouble angle)
    (fromIntegral cx) (fromIntegral cy)

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
