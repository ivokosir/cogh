module Graphics.Cogh.Text
  ( Font
  , newFont
  , deleteFont
  , newTextureFromText
  )where

import Graphics.Cogh.CommonFFI
import Graphics.Cogh.Color

newtype Font = Font (Ptr ())

foreign import ccall unsafe "newFont" newFont'
  :: CString -> CInt -> IO Font
newFont :: FilePath -> Int -> IO Font
newFont file size = withCString file $
  \ cfile -> newFont' cfile $ fromIntegral size

foreign import ccall unsafe "deleteFont" deleteFont
  :: Font -> IO ()

foreign import ccall unsafe "newTextureFromText" cNewTextureFromText
  :: Window -> Font -> CString -> Ptr Float -> IO Texture
newTextureFromText :: Window -> Font -> String -> Color -> IO Texture
newTextureFromText w f text color =
  withCString text $ \ ctext ->
    withColorPtr color $ \ colorPtr ->
      cNewTextureFromText w f ctext colorPtr
