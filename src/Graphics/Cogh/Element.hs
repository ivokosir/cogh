module Graphics.Cogh.Element
  ( module Export
  , Angle
  , Point
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
  , events
  , setEvents
  , emptyElement
  , rectangle
  , group
  , image
  ) where

import Graphics.Cogh.Color as Export hiding (withColorPtr)
import Graphics.Cogh.Render as Export (Texture, textureSize)

import Data.Function
import Graphics.Cogh.Element.Internal
import Graphics.Cogh.Matrix
import Graphics.Cogh.Render
import Graphics.Cogh.Vector
import Graphics.Cogh.Window

position :: Element a -> Vector
position = _position

setPosition :: Vector -> Element a -> Element a
setPosition p e = e {_position = p}

move :: Vector -> Element a -> Element a
move diff e = e {_position = position e + diff}

size :: Element a -> Vector
size = _size

setSize :: Vector -> Element a -> Element a
setSize p e = e {_size = p}

scale :: Element a -> Vector
scale = _scale

setScale :: Vector -> Element a -> Element a
setScale p e = e {_scale = p}

scaleUp :: Vector -> Element a -> Element a
scaleUp s e = e {_scale = scale e * s}

origin :: Element a -> Vector
origin = _origin

setOrigin :: Vector -> Element a -> Element a
setOrigin p e = e {_origin = p}

center :: Element a -> Element a
center e = e {_origin = Point 0.5 0.5}

angle :: Element a -> Angle
angle = _angle

setAngle :: Angle -> Element a -> Element a
setAngle p e = e {_angle = p}

rotate :: Angle -> Element a -> Element a
rotate diff e = e {_angle = angle e + diff}

depth :: Element a -> Float
depth = _depth

setDepth :: Float -> Element a -> Element a
setDepth p e = e {_depth = p}

moveDepth :: Float -> Element a -> Element a
moveDepth diff e = e {_depth = depth e + diff}

events :: Element a -> [Event a]
events = _events

setEvents :: [Event a] -> Element a -> Element a
setEvents es e = e {_events = es}

setRender :: (Window -> Matrix -> IO ()) -> Element a -> Element a
setRender newRender e = e {renderOrChildren = Left newRender}

emptyElement :: Element a
emptyElement =
  Element
  { _position = Point 0 0
  , _size = Point 0 0
  , _scale = Point 1 1
  , _origin = Point 0 0
  , _angle = 0
  , _depth = 0
  , _events = []
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
