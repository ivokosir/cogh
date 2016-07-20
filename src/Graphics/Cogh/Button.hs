module Graphics.Cogh.Button
  ( Button (..)
  , getPressedButtons
  ) where

class Button a where
  isPressed :: a -> Bool

getPressedButtons
  :: (Button a, Eq code) => (a -> code) -> [a] -> [code] -> [code]
getPressedButtons getCode newButtons oldPressedButtons =
  newPressedButtons ++ filter isInNewButtons oldPressedButtons
 where
  isInNewButtons code = code `notElem` map getCode newButtons
  newPressedButtons = getCode <$> filter isPressed newButtons
