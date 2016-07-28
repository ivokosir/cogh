module Graphics.Cogh.Element
  ( module Export
  , Angle, Point, Position, Size, Scale, Origin
  , Element(..)
  , emptyElement
  , rectangle
  , group
  , image
  ) where

import Graphics.Cogh.Color as Export hiding (withColorPtr)
import Graphics.Cogh.Render as Export
  ( Texture
  , textureSize
  )

import Graphics.Cogh.Element.Internal
import Graphics.Cogh.Render
import Graphics.Cogh.Matrix
import Graphics.Cogh.Vector

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
