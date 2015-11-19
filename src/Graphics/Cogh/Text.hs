module Graphics.Cogh.Text
  ( Font
  , newFont
  , deleteFont
  , newTextureFromText
  )where

import Graphics.Cogh.CommonFFI

import Data.Word


newtype Font = Font (Ptr ())

foreign import ccall unsafe "newFont" newFont'
  :: CString -> CInt -> IO Font
newFont :: String -> Int -> IO Font
newFont file size = withCString file $
  \ cfile -> newFont' cfile $ fromIntegral size

foreign import ccall unsafe "deleteFont" deleteFont
  :: Font -> IO ()

foreign import ccall unsafe "newTextureFromText" newTextureFromText'
  :: Window -> Font -> CString -> CUInt -> IO Texture
newTextureFromText :: Window -> Font -> String -> Word32 -> IO Texture
newTextureFromText w f text size = withCString text $
  \ ctext -> newTextureFromText' w f ctext $ CUInt size
