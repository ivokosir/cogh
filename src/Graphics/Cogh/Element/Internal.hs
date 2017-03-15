module Graphics.Cogh.Element.Internal
  ( Element(..)
  , viewMatrix
  , localMatrix
  , normalize
  ) where

import Data.List (sortBy)
import Data.Maybe (catMaybes)
import Graphics.Cogh.Action
import Graphics.Cogh.Matrix
import Graphics.Cogh.Target
import qualified Graphics.Cogh.Vector as V
import Graphics.Cogh.Window.Internal

data Element a = Element
  { _position :: Position
  , _size :: Size
  , _scale :: Scale
  , _origin :: Origin
  , _angle :: Float
  , _depth :: Float
  , _actions :: [Action a]
  , renderOrChildren :: Either (Window -> Matrix -> IO ()) [Element a]
  }

instance Functor Element where
  fmap f e =
    e
    { _actions = (fmap . fmap) f (_actions e)
    , renderOrChildren = (fmap . fmap . fmap) f (renderOrChildren e)
    }

normalize :: V.Pixel -> Element a -> ([Window -> IO a], Window -> IO ())
normalize screenSize e = (actions, renderAll)
  where
    matrix = projection $ fromIntegral <$> screenSize
    unsortedNormalized = normalize' e matrix 0
    compareDepth (_, _, aDepth) (_, _, bDepth) = compare aDepth bDepth
    sortedNormalized = sortBy compareDepth unsortedNormalized
    (actionLists, mRenders) =
      unzip ((\(n, r, _) -> (n, r)) <$> sortedNormalized)
    (actions, renders) = (concat actionLists, catMaybes mRenders)
    renderAll window = sequence_ (fmap (\render -> render window) renders)

normalize'
  :: Element a
  -> Matrix
  -> Float
  -> [([Window -> IO a], Maybe (Window -> IO ()), Float)]
normalize' e parentMatrix parentDepth =
  (actions, mRender, newDepth) : normalizedChildren
  where
    (view, local) = viewAndLocalMatrix e parentMatrix
    newDepth = parentDepth + _depth e
    t = target view local
    actions = fmap (\a window -> runAction a window t) (_actions e)
    normalizeChild child = normalize' child view newDepth
    normalizedChildren =
      case renderOrChildren e of
        Right children -> concatMap normalizeChild children
        _ -> []
    mRender =
      case renderOrChildren e of
        Left render -> Just $ \window -> render window local
        _ -> Nothing

viewAndLocalMatrix :: Element a -> Matrix -> (Matrix, Matrix)
viewAndLocalMatrix e parent = (view, local)
  where
    view =
      mconcat
        [ parent
        , translation (_position e)
        , rotation (_angle e)
        , scaling (_scale e)
        ]
    local = mconcat [view, scaling (_size e), translation (negate $ _origin e)]
