module Graphics.Cogh.Element.Internal
  ( Element (..)
  , EmptyLabel (..)
  , renderRoot
  ) where

import Data.Dynamic
import Data.List (sortBy)
import Graphics.Cogh.Render
import Graphics.Cogh.Matrix
import Graphics.Cogh.Vector
import Graphics.Cogh.Window.CWindow

data EmptyLabel = EmptyLabel deriving (Typeable, Eq)

data Element = Element
  { position' :: Position
  , size' :: Size
  , scale' :: Scale
  , origin' :: Origin
  , angle' :: Angle
  , depth' :: Float
  , label' :: Dynamic
  , normalize
    :: Element -> Matrix -> Float
    -> [(Element, Matrix, Matrix, Float)]
  , render :: WindowPtr -> Matrix -> IO ()
  }

renderRoot :: WindowPtr -> Pixel -> Element -> IO [(Element, Matrix, Matrix)]
renderRoot window screenSize e = do
  clear window
  sequence_ elementRenders
  swapBuffers window
  return interactive
 where
  matrix = projection $ fromIntegral <$> screenSize
  unsortedEs = normalize e e matrix 0
  sortedEs = sortBy sortEs unsortedEs
  sortEs (_, _, _, aDepth) (_, _, _, bDepth) = compare aDepth bDepth
  elementRenders = map getElementRender sortedEs
  getElementRender (element, _, local, _) =
    render element window local
  interactive = toInteractive <$> reverse (filter hasLabel sortedEs)
  toInteractive (element, view, local, _) = (element, view, local)
  hasLabel (element, _, _, _) = fromDynamic (label' element) /= Just EmptyLabel
