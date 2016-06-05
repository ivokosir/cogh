module Graphics.Cogh.Button
  ( Button (..)
  , getPressedButtons
  ) where

class Button a where
  isPressed :: a -> Bool
  isSame :: a -> a -> Bool

getPressedButtons :: Button a => [a] -> [a] -> [a]
getPressedButtons newButtons oldPressedButtons =
  newPressedButtons ++ filter isInNewButtons oldPressedButtons
 where
  isInNewButtons button = not $ any (isSame button) newPressedButtons
  newPressedButtons = filter isPressed newButtons
