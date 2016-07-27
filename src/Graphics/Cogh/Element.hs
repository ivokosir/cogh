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
import Graphics.Cogh.Window
import Graphics.Cogh.Event
import Graphics.Cogh.Matrix hiding (withMatrixPtr)
import Graphics.Cogh.Vector

data Element = Element
  { position :: Position
  , size :: Size
  , scale :: Scale
  , origin :: Origin
  , angle :: Angle
  , depth :: Float
  , normalize :: Element -> Matrix -> Float -> [(Element, Matrix, Float)]
  , render :: Matrix -> Window -> IO ()
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
  rectRender matrix window = drawRect window matrix color

image :: Texture -> Element
image texture = emptyElement
  { size = fromIntegral <$> textureSize texture
  , render = textureRender }
 where
  textureRender matrix window = drawTexture window matrix texture

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

renderRoot :: Window -> Element -> IO ()
renderRoot window e = do
  clear window
  oldState <- getWindowState window
  let
    matrix = projection $ fromIntegral <$> windowSize oldState
    unsortedEs = normalize e e matrix 0
    sortedEs = sortBy sortEs unsortedEs
    sortEs (_, _, aDepth) (_, _, bDepth) = compare aDepth bDepth
    elementRenders = map getElementRender sortedEs
    getElementRender (element, elementMatrix, _) =
      render element elementMatrix window
  sequence_ elementRenders
  swapBuffers window
