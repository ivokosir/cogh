module Graphics.Cogh.Element
  ( module Export
  , Position
  , Size
  , Scale
  , Origin
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
  , actions
  , setActions
  , emptyElement
  , rectangle
  , group
  , image
  ) where

import Graphics.Cogh.Color as Export
import Graphics.Cogh.Render as Export (Texture, textureSize)

import Data.Function
import Graphics.Cogh.Element.Internal
import Graphics.Cogh.Matrix
import Graphics.Cogh.Render
import qualified Graphics.Cogh.Vector as V
import Graphics.Cogh.Window
import Graphics.Cogh.Action

position :: Element a -> V.Vector
position = _position

setPosition :: V.Vector -> Element a -> Element a
setPosition p e = e {_position = p}

move :: V.Vector -> Element a -> Element a
move diff e = e {_position = position e + diff}

size :: Element a -> V.Vector
size = _size

setSize :: V.Vector -> Element a -> Element a
setSize p e = e {_size = p}

scale :: Element a -> V.Vector
scale = _scale

setScale :: V.Vector -> Element a -> Element a
setScale p e = e {_scale = p}

scaleUp :: V.Vector -> Element a -> Element a
scaleUp s e = e {_scale = scale e * s}

origin :: Element a -> V.Vector
origin = _origin

setOrigin :: V.Vector -> Element a -> Element a
setOrigin p e = e {_origin = p}

center :: Element a -> Element a
center e = e {_origin = V.vector 0.5 0.5}

angle :: Element a -> Float
angle = _angle

setAngle :: Float -> Element a -> Element a
setAngle p e = e {_angle = p}

rotate :: Float -> Element a -> Element a
rotate diff e = e {_angle = angle e + diff}

depth :: Element a -> Float
depth = _depth

setDepth :: Float -> Element a -> Element a
setDepth p e = e {_depth = p}

moveDepth :: Float -> Element a -> Element a
moveDepth diff e = e {_depth = depth e + diff}

actions :: Element a -> [Action a]
actions = _actions

setActions :: [Action a] -> Element a -> Element a
setActions as a = a {_actions = as}

setRender :: (Window -> Matrix -> IO ()) -> Element a -> Element a
setRender newRender e = e {renderOrChildren = Left newRender}

emptyElement :: Element a
emptyElement =
  Element
  { _position = V.vector 0 0
  , _size = V.vector 0 0
  , _scale = V.vector 1 1
  , _origin = V.vector 0 0
  , _angle = 0
  , _depth = 0
  , _actions = []
  , renderOrChildren = Right []
  }

rectangle :: Size -> Color -> Element a
rectangle rectSize color =
  emptyElement & setSize rectSize & setRender rectRender
  where
    rectRender window matrix = drawRect window matrix color

image :: Texture -> Element a
image texture =
  emptyElement & setSize (fromIntegral <$> textureSize texture) &
  setRender textureRender
  where
    textureRender window matrix = drawTexture window matrix texture

group :: [Element a] -> Element a
group es = emptyElement {renderOrChildren = Right es}
