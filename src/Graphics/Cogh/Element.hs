module Graphics.Cogh.Element
  ( module Export
  , Angle, Point, Position, Size, Scale, Origin
  , Element
  , position
  , setPosition
  , move
  , size
  , setSize
  , scale
  , setScale
  , scaleUp
  , origin
  , setOrigin
  , center
  , angle
  , setAngle
  , rotate
  , depth
  , setDepth
  , moveDepth
  , label
  , setLabel
  , removeLabel
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

import Data.Dynamic
import Data.Function
import Graphics.Cogh.Element.Internal
import Graphics.Cogh.Render
import Graphics.Cogh.Matrix
import Graphics.Cogh.Vector
import Graphics.Cogh.Window.CWindow

position :: Element -> Vector
position = position'

setPosition :: Vector -> Element -> Element
setPosition p e = e { position' = p }

move :: Vector -> Element -> Element
move diff e = e { position' = position e + diff }

size :: Element -> Vector
size = size'

setSize :: Vector -> Element -> Element
setSize p e = e { size' = p }

scale :: Element -> Vector
scale = scale'

setScale :: Vector -> Element -> Element
setScale p e = e { scale' = p }

scaleUp :: Vector -> Element -> Element
scaleUp s e = e { scale' = scale e * s }

origin :: Element -> Vector
origin = origin'

setOrigin :: Vector -> Element -> Element
setOrigin p e = e { origin' = p }

center :: Element -> Element
center e = e { origin' = Point 0.5 0.5 }

angle :: Element -> Angle
angle = angle'

setAngle :: Angle -> Element -> Element
setAngle p e = e { angle' = p }

rotate :: Angle -> Element -> Element
rotate diff e = e { angle' = angle e + diff }

depth :: Element -> Float
depth = depth'

setDepth :: Float -> Element -> Element
setDepth p e = e { depth' = p }

moveDepth :: Float -> Element -> Element
moveDepth diff e = e { depth' = depth e + diff }

setLabel :: (Typeable a, Eq a) => a -> Element -> Element
setLabel l element = element { label' = toDyn l }
 where
  _ = l == l

label :: (Typeable a, Eq a) => Element -> Maybe a
label element = l
 where
  l = fromDynamic $ label' element
  _ = l == l

removeLabel :: Element -> Element
removeLabel = setLabel EmptyLabel

setRender :: (WindowPtr -> Matrix -> IO ()) -> Element -> Element
setRender newRender e = e { render = newRender }

emptyElement :: Element
emptyElement = Element
  { position' = Point 0 0
  , size' = Point 0 0
  , scale' = Point 1 1
  , origin' = Point 0 0
  , angle' = 0
  , depth' = 0
  , label' = toDyn EmptyLabel
  , normalize = defaultNormalize
  , render = \ _ _ -> return ()
  }

rectangle :: Size -> Color -> Element
rectangle rectSize color = emptyElement
  & setSize rectSize
  & setRender rectRender
 where
  rectRender window matrix = drawRect window matrix color

image :: Texture -> Element
image texture = emptyElement
  & setSize (fromIntegral <$> textureSize texture)
  & setRender textureRender
 where
  textureRender window matrix = drawTexture window matrix texture

group :: [Element] -> Element
group es = emptyElement { normalize = groupNormalize es }

defaultNormalize
  :: Element -> Matrix -> Float
  -> [(Element, Matrix, Matrix, Float)]
defaultNormalize e parentMatrix parentDepth =
  [(e, view, local, parentDepth + depth e)]
 where
  (view, local) = viewAndLocalMatrix e parentMatrix

viewAndLocalMatrix :: Element -> Matrix -> (Matrix, Matrix)
viewAndLocalMatrix e parent = (view, local)
 where
  view = mconcat
    [ parent
    , translation (position e)
    , rotation (angle e)
    , scaling (scale e)
    ]
  local = mconcat
    [ view
    , scaling (size e)
    , translation (negate $ origin e)
    ]

groupNormalize
  :: [Element] -> Element -> Matrix -> Float
  -> [(Element, Matrix, Matrix, Float)]
groupNormalize es e parentMatrix parentDepth =
  (e, view, local, newDepth) :
    concatMap normalizeChild es
 where
  newDepth = parentDepth + depth e
  normalizeChild child = normalize child child view newDepth
  (view, local) = viewAndLocalMatrix e parentMatrix
