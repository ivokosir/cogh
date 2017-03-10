module Graphics.Cogh.Color
  ( Color(..)
  , R
  , G
  , B
  , A
  , red
  , green
  , blue
  , black
  , white
  , transparent
  , withColorPtr
  ) where

import Foreign.Marshal.Array
import Foreign.Ptr

type R = Float

type G = Float

type B = Float

type A = Float

data Color =
  Color R
        G
        B
        A

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

withColorPtr :: Color -> (Ptr Float -> IO a) -> IO a
withColorPtr (Color r g b a) = withArray [r, g, b, a]
