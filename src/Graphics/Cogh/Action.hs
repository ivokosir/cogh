module Graphics.Cogh.Action
  ( Action(..)
  , actionIO
  , actionWindow
  , hover
  , hoverPosition
  , hoverPositionLocal
  , windowSize
  , quit
  , flatten
  ) where

import Control.Applicative
import Control.Monad
import Graphics.Cogh.Action.Internal
import Graphics.Cogh.Action.Mouse
import Graphics.Cogh.Event
import Graphics.Cogh.Target
import Graphics.Cogh.Vector
import Graphics.Cogh.Window.Internal

actionIO :: IO a -> Action a
actionIO io = Action $ \_ _ _ -> (: []) <$> io

actionWindow :: Action Window
actionWindow = Action $ \w _ _ -> return [w]

actionTarget :: Action Target
actionTarget = Action $ \_ _ t -> return [t]

mouseToTarget :: Action (Vector, Vector)
mouseToTarget = do
  pos <- position
  ws <- windowSize
  t <- actionTarget
  let transformed = toTarget t (toWorld ws pos)
  if isInside transformed
    then pure transformed
    else empty

hover :: Action ()
hover = void mouseToTarget

hoverPosition :: Action Vector
hoverPosition = fst <$> mouseToTarget

hoverPositionLocal :: Action Vector
hoverPositionLocal = snd <$> mouseToTarget

quit :: Action ()
quit = flatten $ quits <$> event

windowSize :: Action Pixel
windowSize = flatten $ windowSizes <$> event
