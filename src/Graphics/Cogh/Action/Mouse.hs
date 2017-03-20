module Graphics.Cogh.Action.Mouse
  ( Button
  , code
  , state
  , Code(..)
  , State(..)
  , button
  , motion
  , position
  , scroll
  ) where

import Graphics.Cogh.Action.Internal
import Graphics.Cogh.Event
import Graphics.Cogh.Event.Mouse hiding (motion, position)
import qualified Graphics.Cogh.Event.Mouse as Event
       (motion, position)
import Graphics.Cogh.Vector

button :: Action Button
button = flatten $ mouseButtons <$> event

eventMotion :: Action Motion
eventMotion = flatten $ mouseMotions <$> event

motion :: Action Pixel
motion = Event.motion <$> eventMotion

position :: Action Pixel
position = Event.position <$> eventMotion

scroll :: Action Pixel
scroll = flatten $ mouseScrolls <$> event
