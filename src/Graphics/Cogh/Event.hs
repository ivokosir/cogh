module Graphics.Cogh.Event
  ( Event (..)
  , getEvents
  )where

import Graphics.Cogh.CommonFFI
import Graphics.Cogh.Key
import Graphics.Cogh.Mouse
import Graphics.Cogh.Joystick

import Data.Maybe

getEvents :: Window -> IO [Event]
getEvents w = getEvents' []
 where
  getEvents' es = do
    me <- nextEvent w
    case me of
      Just e -> getEvents' $ e : es
      Nothing -> return es

nextEvent :: Window -> IO (Maybe Event)
nextEvent w = do
  hasNext <- pollEvent w

  if cBool hasNext
    then do
      quit <- maybeQuit w
      resize <- maybeResize w
      key <- maybeKey w
      mouseButton <- maybeMouseButton w
      mousePosition <- maybeMousePosition w
      scroll <- maybeScroll w
      joystickButton <- maybeJoystickButton w
      joystickAxis <- maybeJoystickAxis w

      case listToMaybe $ catMaybes
        [ quit, resize, key, mouseButton , mousePosition
        , scroll, joystickButton, joystickAxis
        ] of
        Nothing -> nextEvent w
        e -> return e
    else return Nothing

 where
  maybeQuit = maybeEvent getQuit $ \ _ -> return Quit

  maybeResize = maybeEvent getResize $ \ p -> do
    x <- resizeW p
    y <- resizeH p
    return $ Resize (fromIntegral x) (fromIntegral y)


foreign import ccall unsafe "pollEvent" pollEvent
  :: Window -> IO (CInt)


foreign import ccall unsafe "getQuit" getQuit
  :: Window -> IO (Ptr ())


foreign import ccall unsafe "getResize" getResize
  :: Window -> IO (Ptr ())

foreign import ccall unsafe "resizeW" resizeW
  :: Ptr () -> IO CUInt

foreign import ccall unsafe "resizeH" resizeH
  :: Ptr () -> IO CUInt
