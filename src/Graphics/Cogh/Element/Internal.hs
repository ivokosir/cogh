module Graphics.Cogh.Element.Internal
  ( Element(..)
  , renderRoot
  ) where

import Data.List (sortBy)
import Graphics.Cogh.Render
import Graphics.Cogh.Matrix
import Graphics.Cogh.Vector
import Graphics.Cogh.Window.CWindow

data Element = Element
  { position :: Position
  , size :: Size
  , scale :: Scale
  , origin :: Origin
  , angle :: Angle
  , depth :: Float
  , normalize :: Element -> Matrix -> Float -> [(Element, Matrix, Float)]
  , render :: WindowPtr -> Matrix -> IO ()
  }

renderRoot :: WindowPtr -> Pixel -> Element -> IO ()
renderRoot window screenSize e = do
  clear window
  sequence_ elementRenders
  swapBuffers window
 where
  matrix = projection $ fromIntegral <$> screenSize
  unsortedEs = normalize e e matrix 0
  sortedEs = sortBy sortEs unsortedEs
  sortEs (_, _, aDepth) (_, _, bDepth) = compare aDepth bDepth
  elementRenders = map getElementRender sortedEs
  getElementRender (element, elementMatrix, _) =
    render element window elementMatrix
