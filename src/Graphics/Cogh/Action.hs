module Graphics.Cogh.Action
  ( Action
  , action
  , runAction
  ) where

import Graphics.Cogh.Target (Target)
import Graphics.Cogh.Window.Internal (Window)

newtype Action a =
  Action (Window -> Target -> IO a)

action :: (Window -> Target -> IO a) -> Action a
action = Action

runAction :: Action a -> Window -> Target -> IO a
runAction (Action a) = a

instance Functor Action where
  fmap f a = action $ \window target -> fmap f (runAction a window target)
