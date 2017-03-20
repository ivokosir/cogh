module Graphics.Cogh.Action.Keyboard
  ( Key
  , code
  , readCode
  , showCode
  , state
  , State(..)
  , key
  ) where

import Graphics.Cogh.Action.Internal
import Graphics.Cogh.Event
import Graphics.Cogh.Event.Keyboard

key :: Action Key
key = flatten $ keys <$> event
