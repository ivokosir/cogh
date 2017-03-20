module Graphics.Cogh.Action.Internal
  ( Action(..)
  , event
  , flatten
  ) where

import Control.Applicative
import Control.Monad
import Graphics.Cogh.Event (Events)
import Graphics.Cogh.Target (Target)
import Graphics.Cogh.Window.Internal (Window)

newtype Action a =
  Action (Window -> Events -> Target -> IO [a])

instance Functor Action where
  fmap f (Action a) = Action $ \w e t -> (fmap . fmap) f (a w e t)

instance Applicative Action where
  pure x = Action $ \_ _ _ -> return [x]
  (Action af) <*> (Action ax) =
    Action $ \w e t -> do
      fs <- af w e t
      xs <- ax w e t
      return (fs <*> xs)

instance Alternative Action where
  empty = Action $ \_ _ _ -> return []
  (Action a1) <|> (Action a2) =
    Action $ \w e t -> do
      results1 <- a1 w e t
      results2 <- a2 w e t
      return (results1 ++ results2)

instance Monad Action where
  (Action ax) >>= xToAy =
    Action $ \w e t -> do
      xs <- ax w e t
      let runAy x = ay w e t
            where
              (Action ay) = xToAy x
      ys <- sequence $ runAy <$> xs
      return $ concat ys

instance MonadPlus Action where
  mzero = empty
  mplus = (<|>)

instance Monoid (Action a) where
  mempty = empty
  mappend = (<|>)

event :: Action Events
event = Action $ \_ e _ -> return [e]

flatten
  :: (Foldable t, Alternative m, Monad m)
  => m (t a) -> m a
flatten = (=<<) (foldr ((<|>) . pure) empty)
