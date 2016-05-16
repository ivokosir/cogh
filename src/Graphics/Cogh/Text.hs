module Graphics.Cogh.Text
  ( Font
  , newFont
  , fontSize
  , newTextureFromText
  )where

import Foreign.C
import Foreign.Ptr
import Foreign.ForeignPtr
import Graphics.Cogh.Color
import Graphics.Cogh.Render
import Graphics.Cogh.Window.Internal

data Font = Font (ForeignPtr ()) Int

newFont :: FilePath -> Int -> IO Font
newFont file size = do
  cFont <- withCString file $ \ cFile -> cNewFont cFile $ fromIntegral size
  foreignPtr <- newForeignPtr deleteFontFunPtr cFont
  return $ Font foreignPtr size

fontSize :: Font -> Int
fontSize (Font _ size) = size

withCFont :: Font -> (Ptr () -> IO a) -> IO a
withCFont (Font foreignPtr _) = withForeignPtr foreignPtr

foreign import ccall unsafe "newFont" cNewFont
  :: CString -> CInt -> IO (Ptr ())

foreign import ccall unsafe "&deleteFont" deleteFontFunPtr
  :: FunPtr (Ptr () -> IO ())

newTextureFromText :: Window -> Font -> String -> Color -> IO Texture
newTextureFromText w f text color = do
  cTexture <-
    withCFont f $ \ cFont ->
      withCString text $ \ cText ->
        withColorPtr color $ \ cColor ->
          cNewTextureFromText w cFont cText cColor
  newTexture cTexture

foreign import ccall unsafe "newTextureFromText" cNewTextureFromText
  :: Window -> Ptr () -> CString -> Ptr Float -> IO (Ptr ())
