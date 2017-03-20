module Graphics.Cogh.Color
  ( Color(..)
  , color
  , colorAlpha
  , red
  , green
  , blue
  , black
  , white
  , transparent
  ) where

data Color =
  Color Float
        Float
        Float
        Float

color :: Float -> Float -> Float -> Color
color r g b = Color r g b 1

colorAlpha :: Float -> Float -> Float -> Float -> Color
colorAlpha = Color

red :: Color
red = Color 1 0 0 1

green :: Color
green = Color 0 1 0 1

blue :: Color
blue = Color 0 0 1 1

black :: Color
black = Color 0 0 0 1

white :: Color
white = Color 1 1 1 1

transparent :: Color
transparent = Color 0 0 0 0
