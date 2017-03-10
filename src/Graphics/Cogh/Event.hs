module Graphics.Cogh.Event
  ( Event
  , pollEvents
  ) where

import qualified Graphics.Cogh.Event.Keyboard as Keyboard
import qualified Graphics.Cogh.Event.Mouse as Mouse
import qualified Graphics.Cogh.Event.Window as Window
import Graphics.Cogh.Window.Internal

data Event
  = Keyboard Keyboard.Event
  | Mouse Mouse.Event
  | Window Window.Event

pollEvents :: Window -> IO [Event]
pollEvents w = do
  cPollEvents w
  keyboardEvents <- Keyboard.getEvents w
  mouseEvents <- Mouse.getEvents w
  windowEvents <- Window.getEvents w
  return $
    concat
      [ fmap Keyboard keyboardEvents
      , fmap Mouse mouseEvents
      , fmap Graphics.Cogh.Event.Window windowEvents
      ]

foreign import ccall unsafe "pollEvents" cPollEvents ::
               Window -> IO ()
