module Graphics.Cogh.Action.Mouse
  ( Button
  , code
  , state
  , Code(..)
  , State(..)
  , button
  , lmb
  , rmb
  , position
  , motion
  , scroll
  ) where

import Control.Monad
import Graphics.Cogh.Action.Internal
import Graphics.Cogh.Event
import Graphics.Cogh.Event.Mouse hiding (motion, position)
import qualified Graphics.Cogh.Event.Mouse as Event
       (motion, position)
import Graphics.Cogh.Vector
import Prelude hiding (Left, Right)

button :: Action Button
button = flatten $ mouseButtons <$> event

lmb :: Action State
lmb = state <$> mfilter (\b -> code b == Left) button

rmb :: Action State
rmb = state <$> mfilter (\b -> code b == Right) button

eventMotion :: Action Motion
eventMotion = flatten $ mouseMotions <$> event

position :: Action Pixel
position = Event.position <$> eventMotion

motion :: Action Pixel
motion = Event.motion <$> eventMotion

scroll :: Action Pixel
scroll = flatten $ mouseScrolls <$> event
