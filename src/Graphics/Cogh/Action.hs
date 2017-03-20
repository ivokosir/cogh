module Graphics.Cogh.Action
  ( Action(..)
  , actionIO
  , actionWindow
  , actionTarget
  , windowSize
  , quit
  , flatten
  ) where

import Graphics.Cogh.Action.Internal
import Graphics.Cogh.Event
import Graphics.Cogh.Target (Target)
import Graphics.Cogh.Vector (Pixel)
import Graphics.Cogh.Window.Internal

actionIO :: IO a -> Action a
actionIO io = Action $ \_ _ _ -> (: []) <$> io

actionWindow :: Action Window
actionWindow = Action $ \w _ _ -> return [w]

actionTarget :: Action Target
actionTarget = Action $ \_ _ t -> return [t]

quit :: Action ()
quit = flatten $ quits <$> event

windowSize :: Action Pixel
windowSize = flatten $ windowSizes <$> event
