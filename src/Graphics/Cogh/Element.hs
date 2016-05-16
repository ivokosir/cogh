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

import qualified Graphics.Cogh.Color as Export hiding (withColorPtr)
import qualified Graphics.Cogh.Render as Export
  ( Texture
  , textureWidth
  , textureHeight
  )

import Graphics.Cogh.Render
import Graphics.Cogh.Window
import Graphics.Cogh.Matrix
import Graphics.Cogh.Color

data Element = Element
  { position :: Position
  , size :: Size
  , scale :: Scale
  , origin :: Origin
  , angle :: Angle
  , render :: Element -> Matrix -> Window -> IO ()
  }

emptyElement :: Element
emptyElement = Element
  { position = (0, 0)
  , size = (0, 0)
  , scale = (1, 1)
  , origin = (0, 0)
  , angle = 0
  , render = \ _ _ _ -> return ()
  }

-- mouseArea :: IO Element
-- mouseArea = do
--   clickRef <- newIORef Nothing
--   resultRef <- newIORef False
--   return emptyElement { render = mouseUpdate clickRef resultRef }
--  where
--   mouseUpdate clickRef resultRef e view _ = do
--     click <- readIORef clickRef
--     case click of
--       Just point -> writeIORef resultRef (isInMatrix point matrix)
--       Nothing -> return ()
--    where
--     isInMatrix _ _ = False
--     matrix = mconcat
--       [ view
--       , translation (position e)
--       , rotation (angle e)
--       , scaling (scale e)
--       , scaling (size e)
--       , translation ((\ (x, y) -> (-x, -y)) (origin e))
--       ]

rectangle :: Size -> Color -> Element
rectangle rectSize color = emptyElement { size = rectSize, render = rectRender }
 where
  rectRender e view window =
    withMatrixPtr matrix $ \ mPtr ->
      withColorPtr color $ \ colorPtr ->
        drawRect window mPtr colorPtr
   where
    matrix = mconcat
      [ view
      , translation (position e)
      , rotation (angle e)
      , scaling (scale e)
      , scaling (size e)
      , translation ((\ (x, y) -> (-x, -y)) (origin e))
      ]

image :: Size -> Texture -> Element
image rectSize texture = emptyElement { size = rectSize, render = textureRender }
 where
  textureRender e view window =
    withMatrixPtr matrix $ \ mPtr ->
      drawTexture window mPtr texture
   where
    matrix = mconcat
      [ view
      , translation (position e)
      , rotation (angle e)
      , scaling (scale e)
      , scaling (size e)
      , translation ((\ (x, y) -> (-x, -y)) (origin e))
      ]

-- modelMatrix :: Element -> Matrix
-- modelMatrix e = mconcat
--   [ translation (position e)
--   , rotation (angle e)
--   , scaling (scale e)
--   , scaling (size e)
--   , translation ((\ (x, y) -> (-x, -y)) (origin e))
--   ]

group :: [Element] -> Element
group es = emptyElement { render = renderGroup }
 where
  renderGroup e view window = sequence_ (fmap renderChild es)
   where
    renderChild child = render child child matrix window
    matrix = mconcat
      [ view
      , translation (position e)
      , rotation (angle e)
      , scaling (scale e)
      ]

renderRoot :: Window -> Point -> Element -> IO ()
renderRoot window renderArea e = do
  clear window
  render e e matrix window
  swapBuffers window
 where
  matrix = projection renderArea
