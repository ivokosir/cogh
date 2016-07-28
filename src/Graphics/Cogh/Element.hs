module Graphics.Cogh.Element
  ( module Export
  , Angle, Point, Position, Size, Scale, Origin
  , Element(..)
  , emptyElement
  , rectangle
  , group
  , image
  , renderRoot
  ) where

import Graphics.Cogh.Color as Export hiding (withColorPtr)
import Graphics.Cogh.Render as Export
  ( Texture
  , textureSize
  )

import Data.List (sortBy)
import Graphics.Cogh.Render
import Graphics.Cogh.Matrix hiding (withMatrixPtr)
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

emptyElement :: Element
emptyElement = Element
  { position = Point 0 0
  , size = Point 0 0
  , scale = Point 1 1
  , origin = Point 0 0
  , angle = 0
  , depth = 0
  , normalize = defaultNormalize
  , render = \ _ _ -> return ()
  }

defaultNormalize :: Element -> Matrix -> Float -> [(Element, Matrix, Float)]
defaultNormalize e parentMatrix parentDepth =
  [(e, defaultMatrix e parentMatrix, parentDepth + depth e)]

defaultMatrix :: Element -> Matrix -> Matrix
defaultMatrix e view = mconcat
  [ view
  , translation (position e)
  , rotation (angle e)
  , scaling (scale e)
  , scaling (size e)
  , translation (negate $ origin e)
  ]

rectangle :: Size -> Color -> Element
rectangle rectSize color = emptyElement
  { size = rectSize
  , render = rectRender }
 where
  rectRender window matrix = drawRect window matrix color

image :: Texture -> Element
image texture = emptyElement
  { size = fromIntegral <$> textureSize texture
  , render = textureRender }
 where
  textureRender window matrix = drawTexture window matrix texture

group :: [Element] -> Element
group es = emptyElement { normalize = groupNormalize es }

groupMatrix :: Element -> Matrix -> Matrix
groupMatrix e view = mconcat
  [ view
  , translation (position e)
  , rotation (angle e)
  , scaling (scale e)
  ]

groupNormalize
  :: [Element] -> Element -> Matrix -> Float
  -> [(Element, Matrix, Float)]
groupNormalize es e parentMatrix parentDepth =
  defaultNormalize e parentMatrix parentDepth ++
    concatMap normalizeChild es
 where
  newMatrix = groupMatrix e parentMatrix
  newDepth = parentDepth + depth e
  normalizeChild child = normalize child child newMatrix newDepth

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
