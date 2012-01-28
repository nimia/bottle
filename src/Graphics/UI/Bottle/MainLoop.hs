{-# OPTIONS -Wall #-}
module Graphics.UI.Bottle.MainLoop (mainLoopAnim, mainLoopImage, mainLoopWidget) where

import Control.Arrow(second, (&&&), (***))
import Control.Concurrent(forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Exception(SomeException, try, throwIO)
import Control.Monad(forever, liftM)
import Data.IORef
import Data.Monoid (mappend)
import Data.StateVar (($=))
import Data.Time.Clock (NominalDiffTime, getCurrentTime, diffUTCTime)
import Data.Vector.Vector2 (Vector2(..))
import Graphics.DrawingCombinators ((%%))
import Graphics.DrawingCombinators.Utils (Image, drawText, textSize)
import Graphics.UI.Bottle.EventMap (Event)
import Graphics.UI.Bottle.SizeRange (Size)
import Graphics.UI.Bottle.Widget(Widget)
import Graphics.UI.GLFW (defaultDisplayOptions, getWindowDimensions)
import Graphics.UI.GLFW.Events (GLFWEvent(..), eventLoop)
import qualified Data.Vector.Vector2 as Vector2
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.Rendering.OpenGL.GL as GL
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.GLFW.Utils as GLFWUtils

-- ^ Returns a function that makes all of the given IO actions execute
-- in the same new thread.
coalsceToThread :: IO (IO () -> IO ())
coalsceToThread = do
  requestVar <- newEmptyMVar
  _ <- forkIO . forever $ do
    (action, responseVar) <- takeMVar requestVar
    result <- try action :: IO (Either SomeException ())
    putMVar responseVar result
  let
    runAction action = do
      responseVar <- newEmptyMVar
      putMVar requestVar (action, responseVar)
      either throwIO return =<< takeMVar responseVar

  return runAction

inAnotherThread :: (IO () -> IO ()) -> IO a -> IO a
inAnotherThread coalesce action = do
  m <- newEmptyMVar
  coalesce (action >>= putMVar m)
  takeMVar m

timeBetweenInvocations ::
  IO ((Maybe NominalDiffTime -> IO a) -> IO a)
timeBetweenInvocations = do
  mLastInvocationTimeVar <- newMVar Nothing
  return $ modifyMVar mLastInvocationTimeVar . updateTime
  where
    updateTime f mLastInvocationTime = do
      currentTime <- getCurrentTime
      let
        mTimeSince =
          fmap (currentTime `diffUTCTime`) mLastInvocationTime
      result <- f mTimeSince
      return (Just currentTime, result)

mainLoopImage ::
  Draw.Font -> (Size -> Event -> IO Bool) ->
  (Bool -> Size -> IO (Maybe Image)) -> IO a
mainLoopImage fpsFont eventHandler makeImage = GLFWUtils.withGLFW $ do
  decorateIO <- coalsceToThread
  let coalesce = inAnotherThread decorateIO

  addDelayArg <- timeBetweenInvocations
  GLFWUtils.openWindow defaultDisplayOptions

  let
    windowSize = do
      (x, y) <- getWindowDimensions
      return $ Vector2 (fromIntegral x) (fromIntegral y)

    eventHandlerWithSize size event = coalesce $ eventHandler size event
  let
    handleEvent size (GLFWKeyEvent keyEvent) =
      eventHandlerWithSize size keyEvent
    handleEvent _ GLFWWindowClose =
      error "Quit" -- TODO: Make close event
    handleEvent _ GLFWWindowRefresh = return True

    useFont = drawText fpsFont &&& textSize fpsFont
    scale w h = (Draw.scale w h %%) *** (* Vector2 w h)
    placeAt winSize ratio (image, size) =
      Draw.translate
      (Vector2.uncurry (,) (ratio * (winSize - size))) %% image
    addDelayToImage winSize mkMImage = addDelayArg $ \mTimeSince -> do
      mImage <- mkMImage
      return $
        fmap
          (mappend . placeAt winSize (Vector2 0 1) . scale 20 20 .
           useFont . maybe "N/A" (show . (1/)) $
           mTimeSince)
          mImage

    handleEvents events = do
      winSize@(Vector2 winSizeX winSizeY) <- windowSize
      anyChange <- fmap or $ mapM (handleEvent winSize) events
      GL.viewport $=
        (GL.Position 0 0,
         GL.Size (round winSizeX) (round winSizeY))
      mNewImage <-
        coalesce . addDelayToImage winSize $
        makeImage anyChange winSize
      case mNewImage of
        Nothing -> threadDelay 10000
        Just image ->
          Draw.clearRender .
          (Draw.translate (-1, 1) %%) .
          (Draw.scale (2/winSizeX) (-2/winSizeY) %%) $
          image
  eventLoop handleEvents

mainLoopAnim ::
  Draw.Font -> (Size -> Event -> IO (Maybe Widget.EventResult)) ->
  (Size -> IO Anim.Frame) -> IO a
mainLoopAnim fpsFont eventHandler makeFrame = do
  frameStateVar <- newIORef Nothing
  let
    makeImage isChange size = do
      frameState <- readIORef frameStateVar

      newFrameState <-
        case frameState of
          Nothing -> fmap (Just . (,) 0) $ makeFrame size
          Just (drawCount, prevFrame) ->
            if drawCount == 0 || isChange
              then do
                dest <- makeFrame size
                return . Just $
                  case Anim.nextFrame dest prevFrame of
                    Nothing -> (drawCount + 1, dest)
                    Just newFrame -> (0 :: Int, newFrame)
              else
                return $ Just (drawCount + 1, prevFrame)
      writeIORef frameStateVar newFrameState
      return $
        case newFrameState of
          Nothing -> error "No frame to draw at start??"
          Just (drawCount, frame)
            | drawCount < stopAtDrawCount -> Just (Anim.draw frame)
            | otherwise -> Nothing
    -- A note on draw counts:
    -- When a frame is dis-similar to the previous the count resets to 0
    -- When a frame is similar and animation stops the count becomes 1
    -- We then should draw it again (for double buffering issues) at count 2
    -- And stop drawing it at count 3.
    stopAtDrawCount = 3
    imgEventHandler size event = do
      mEventResult <- eventHandler size event
      case mEventResult of
        Nothing -> return False
        Just eventResult -> do
          modifyIORef frameStateVar . fmap . second . Anim.mapIdentities $ Widget.eAnimIdMapping eventResult
          return True
  mainLoopImage fpsFont imgEventHandler makeImage

mainLoopWidget :: Draw.Font -> IO (Widget IO) -> IO a
mainLoopWidget fpsFont mkWidget =
  mainLoopAnim fpsFont eventHandler mkImage
  where
    eventHandler size event = do
      widget <- mkWidget
      maybe (return Nothing) (liftM Just) .
        E.lookup event $ Widget.eventMap widget size
    mkImage size = do
      widget <- mkWidget
      return $ Widget.image widget size
