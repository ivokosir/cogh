module Graphics.Cogh.Element.Internal
  ( Element(..)
  , Event(..)
  , Target
  , viewMatrix
  , localMatrix
  , normalize
  ) where

import Data.List (sortBy)
import Data.Maybe (catMaybes)
import Graphics.Cogh.Matrix
import Graphics.Cogh.Vector
import Graphics.Cogh.Window.Internal

data Element a = Element
  { _position :: Position
  , _size :: Size
  , _scale :: Scale
  , _origin :: Origin
  , _angle :: Angle
  , _depth :: Float
  , _events :: [Event a]
  , renderOrChildren :: Either (Window -> Matrix -> IO ()) [Element a]
  }

instance Functor Element where
  fmap f e =
    e
    { _events = (fmap . fmap) f (_events e)
    , renderOrChildren = (fmap . fmap . fmap) f (renderOrChildren e)
    }

newtype Event a = Event
  { runEvent :: Window -> Target -> IO a
  }

instance Functor Event where
  fmap f (Event event) = Event $ \window target -> fmap f (event window target)

data Target =
  Target Matrix
         Matrix

viewMatrix :: Target -> Matrix
viewMatrix (Target view _) = view

localMatrix :: Target -> Matrix
localMatrix (Target _ local) = local

normalize :: Pixel -> Element a -> ([Window -> IO a], Window -> IO ())
normalize screenSize e = (events, renderAll)
  where
    matrix = projection $ fromIntegral <$> screenSize
    unsortedNormalized = normalize' e matrix 0
    compareDepth (_, _, aDepth) (_, _, bDepth) = compare aDepth bDepth
    sortedNormalized = sortBy compareDepth unsortedNormalized
    (eventLists, mRenders) = unzip ((\(n, r, _) -> (n, r)) <$> sortedNormalized)
    (events, renders) = (concat eventLists, catMaybes mRenders)
    renderAll window = sequence_ (fmap (\render -> render window) renders)

normalize'
  :: Element a
  -> Matrix
  -> Float
  -> [([Window -> IO a], Maybe (Window -> IO ()), Float)]
normalize' e parentMatrix parentDepth =
  (events, mRender, newDepth) : normalizedChildren
  where
    (view, local) = viewAndLocalMatrix e parentMatrix
    newDepth = parentDepth + _depth e
    target = Target view local
    events = fmap (\(Event event) window -> event window target) (_events e)
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
