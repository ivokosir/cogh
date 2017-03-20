module Graphics.Cogh.Element.Internal
  ( Element
  , emptyElement
  , position
  , size
  , scale
  , origin
  , rotation
  , depth
  , action
  , render
  , children
  , normalize
  ) where

import Data.List (sortBy)
import Graphics.Cogh.Action.Internal
import Graphics.Cogh.Event
import Graphics.Cogh.Matrix hiding (rotation)
import qualified Graphics.Cogh.Matrix as Matrix (rotation)
import Graphics.Cogh.Target
import Graphics.Cogh.Vector (vector)
import Graphics.Cogh.Window.Internal
import Lens.Micro
import Lens.Micro.TH

data Element a = Element
  { _position :: Position
  , _size :: Size
  , _scale :: Scale
  , _origin :: Origin
  , _rotation :: Float
  , _depth :: Float
  , _action :: Action a
  , _render :: Window -> Matrix -> IO ()
  , _children :: [Element a]
  }

makeLenses ''Element

emptyElement :: Element a
emptyElement =
  Element
    (vector 0 0)
    (vector 0 0)
    (vector 1 1)
    (vector 0 0)
    0
    0
    mempty
    (\_ _ -> return ())
    []

instance Functor Element where
  fmap f e =
    e {_action = fmap f (_action e), _children = (fmap . fmap) f (_children e)}

normalize :: Window -> Events -> Element a -> Matrix -> ([IO [a]], IO ())
normalize window events element matrix = (results, renderAll)
  where
    unsortedNormalized = normalize' window events element matrix 0
    compareDepth (_, _, aDepth) (_, _, bDepth) = compare aDepth bDepth
    sortedNormalized = sortBy compareDepth unsortedNormalized
    (results, renders) = unzip ((\(n, r, _) -> (n, r)) <$> sortedNormalized)
    renderAll = sequence_ renders

normalize' :: Window
           -> Events
           -> Element a
           -> Matrix
           -> Float
           -> [(IO [a], IO (), Float)]
normalize' window events element parentMatrix parentDepth =
  (results, (element ^. render) window local, newDepth) : normalizedChildren
  where
    (view, local) = viewAndLocalMatrix element parentMatrix
    (Action a) = element ^. action
    newDepth = parentDepth + element ^. depth
    results = a window events (Target view local)
    normalizeChild child = normalize' window events child view newDepth
    normalizedChildren = concatMap normalizeChild (element ^. children)

viewAndLocalMatrix :: Element a -> Matrix -> (Matrix, Matrix)
viewAndLocalMatrix e parent = (view, local)
  where
    view =
      mconcat
        [ parent
        , translation (e ^. position)
        , Matrix.rotation (e ^. rotation)
        , scaling (e ^. scale)
        ]
    local =
      mconcat [view, scaling (e ^. size), translation (negate $ e ^. origin)]
