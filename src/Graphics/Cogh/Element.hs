module Graphics.Cogh.Element
  ( module Export
  , Position
  , Size
  , Scale
  , Origin
  , Element
  , position
  , size
  , scale
  , origin
  , rotation
  , depth
  , action
  , emptyElement
  , rectangle
  , group
  , image
  ) where

import Graphics.Cogh.Color as Export
import Graphics.Cogh.Render as Export (Texture, textureSize)

import Data.Function
import Graphics.Cogh.Element.Internal
import Graphics.Cogh.Matrix (Position, Size, Scale, Origin)
import Graphics.Cogh.Render
import Graphics.Cogh.Vector (toVector)
import Lens.Micro

rectangle :: Size -> Color -> Element a
rectangle rectSize c = emptyElement & size .~ rectSize & render .~ rectRender
  where
    rectRender window matrix = drawRect window matrix c

image :: Texture -> Element a
image texture =
  emptyElement & size .~ (texture & textureSize & toVector) & render .~
  textureRender
  where
    textureRender window matrix = drawTexture window matrix texture

group :: [Element a] -> Element a
group es = emptyElement & children .~ es
