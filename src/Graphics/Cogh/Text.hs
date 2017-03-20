module Graphics.Cogh.Text
  ( Font
  , newFont
  , fontSize
  , newTextureFromText
  ) where

import Foreign.C
import Foreign.ForeignPtr
import Foreign.Ptr
import Graphics.Cogh.Color
import Graphics.Cogh.Render
import Graphics.Cogh.Window.Internal

data Font =
  Font (ForeignPtr ())
       Int

type FontPtr = Ptr ()

type TextPtr = Ptr ()

newFont :: FilePath -> Int -> IO Font
newFont file size = do
  cFont <- withCString file $ \cFile -> cNewFont cFile $ fromIntegral size
  foreignPtr <- newForeignPtr deleteFontFunPtr cFont
  return $ Font foreignPtr size

fontSize :: Font -> Int
fontSize (Font _ size) = size

withCFont :: Font -> (FontPtr -> IO a) -> IO a
withCFont (Font foreignPtr _) = withForeignPtr foreignPtr

foreign import ccall unsafe "newFont" cNewFont ::
               CString -> CInt -> IO FontPtr

foreign import ccall unsafe "&deleteFont" deleteFontFunPtr ::
               FunPtr (FontPtr -> IO ())

newTextureFromText :: Window -> Font -> String -> Color -> IO Texture
newTextureFromText w f text c = do
  cTexture <-
    withCFont f $ \cFont ->
      withCString text $ \cText ->
        withColorPtr c $ \cColor -> cNewTextureFromText w cFont cText cColor
  newTexture cTexture

foreign import ccall unsafe "newTextureFromText"
               cNewTextureFromText ::
               Window -> FontPtr -> CString -> Ptr Float -> IO TextPtr
