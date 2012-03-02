{-# OPTIONS -Wall #-}
{-# LANGUAGE Rank2Types, DeriveFunctor, FlexibleInstances, MultiParamTypeClasses, TemplateHaskell, GeneralizedNewtypeDeriving #-}
module Graphics.UI.Bottle.Widget (
  Widget(..), MEnter, R,
  EnterResult(..), atEnterResultEvent, atEnterResultRect,
  Id(..), atId, joinId, subId,
  UserIO(..), atUioMaybeEnter, atUioEventMap, atUioFrame, atUioFocalArea,
  EventM, liftEventM, runEventM, addAnimIdMapping, atEventMAction,
  EventResult(..), atErCursor, emptyEventResult, eventResultFromCursor,
  actionEventMap, actionEventMapMovesCursor,
  EventHandlers, atContent, atIsFocused,
  userIO, image, eventMap,
  takesFocus, atMkUserIO, atUserIO,
  atImageWithSize, atImage, atMaybeEnter, atEventMap, atEvents,
  backgroundColor, tint, liftView,
  strongerEvents, weakerEvents,
  translate, translateUserIO, align) where

import Control.Applicative (Applicative(..), (<$))
import Control.Arrow (second)
import Control.Monad.Trans.Class (MonadTrans)
import Control.Monad.Trans.Writer (WriterT(..), mapWriterT)
import Data.Binary (Binary)
import Data.List(isPrefixOf)
import Data.Monoid (Monoid(..))
import Data.Vector.Vector2 (Vector2)
import Graphics.UI.Bottle.Animation (AnimId, R)
import Graphics.UI.Bottle.Direction (Direction)
import Graphics.UI.Bottle.EventMap (EventMap)
import Graphics.UI.Bottle.Rect (Rect(..))
import Graphics.UI.Bottle.SizeRange (Size)
import Graphics.UI.Bottle.Sized (Sized)
import qualified Data.AtFieldTH as AtFieldTH
import qualified Data.Monoid as Monoid
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.Direction as Direction
import qualified Graphics.UI.Bottle.EventMap as EventMap
import qualified Graphics.UI.Bottle.Rect as Rect
import qualified Graphics.UI.Bottle.Sized as Sized

argument :: (a -> b) -> (b -> c) -> a -> c
argument = flip (.)

newtype Id = Id {
  toAnimId :: AnimId
  }
  deriving (Eq, Ord, Show, Read, Binary, Monoid)

joinId :: Id -> AnimId -> Id
joinId (Id x) y = Id $ x ++ y

subId :: Id -> Id -> Maybe AnimId
subId (Id folder) (Id path)
  | folder `isPrefixOf` path = Just $ drop (length folder) path
  | otherwise = Nothing

type AnimIdMapping = Monoid.Endo AnimId
newtype EventM f a = EventM {
  emWriterT :: WriterT AnimIdMapping f a
  }
  deriving (Functor, Applicative, Monad, MonadTrans)

newtype EventResult = EventResult {
  erCursor :: Maybe Id
  }

data EnterResult f = EnterResult {
  enterResultRect :: Rect,
  enterResultEvent :: EventM f EventResult
  }

type MEnter f = Maybe (Direction -> EnterResult f)
type EventHandlers f = EventMap (EventM f EventResult)

data UserIO f = UserIO {
  uioFrame :: Anim.Frame,
  uioMaybeEnter :: MEnter f, -- Nothing if we're not enterable
  uioEventMap :: EventHandlers f,
  uioFocalArea :: Rect
  }

data Widget f = Widget {
  isFocused :: Bool,
  content :: Sized (UserIO f)
  }

AtFieldTH.make ''EventM
AtFieldTH.make ''EnterResult
AtFieldTH.make ''EventResult
AtFieldTH.make ''Id
AtFieldTH.make ''UserIO
AtFieldTH.make ''Widget

toEventM :: f (a, AnimIdMapping) -> EventM f a
toEventM = EventM . WriterT

liftEventM :: Functor f => f a -> EventM f a
liftEventM = toEventM . fmap (flip (,) mempty)

runEventM :: Functor f => EventM f a -> f (a, AnimId -> AnimId)
runEventM =
  (fmap . second) Monoid.appEndo . runWriterT . emWriterT

atEventMAction
  :: (forall a. f a -> g a) -> EventM f b -> EventM g b
atEventMAction f = atEmWriterT . mapWriterT $ f

mkEventM
  :: Functor f => (AnimId -> AnimId) -> f a -> EventM f a
mkEventM mapping = toEventM . fmap (flip (,) (Monoid.Endo mapping))

addAnimIdMapping :: Applicative f => (AnimId -> AnimId) -> EventM f ()
addAnimIdMapping mapping = mkEventM mapping $ pure ()

emptyEventResult :: Functor f => f () -> EventM f EventResult
emptyEventResult = mkEventM id . (EventResult Nothing <$)

eventResultFromCursor :: Functor f => f Id -> EventM f EventResult
eventResultFromCursor = mkEventM id . fmap (EventResult . Just)

atEvents
  :: (EventM f EventResult -> EventM g EventResult)
  -> Widget f -> Widget g
atEvents func =
  atUserIO chg
  where
    chg userIo = userIo {
      uioMaybeEnter =
        (fmap . fmap . atEnterResultEvent) func $
        uioMaybeEnter userIo,
      uioEventMap = fmap func $ uioEventMap userIo
      }

liftView :: Sized Anim.Frame -> Widget f
liftView view =
  Widget {
    isFocused = False,
    content = Sized.atFromSize buildUserIO view
    }
  where
    buildUserIO mkFrame size =
      UserIO {
        uioFocalArea = Rect 0 size,
        uioFrame = mkFrame size,
        uioEventMap = mempty,
        uioMaybeEnter = Nothing
        }

atUserIO :: (UserIO f -> UserIO g) -> Widget f -> Widget g
atUserIO = atContent . fmap

atMkUserIO :: ((Size -> UserIO f) -> Size -> UserIO f) -> Widget f -> Widget f
atMkUserIO = atContent . Sized.atFromSize

atImageWithSize :: (Size -> Anim.Frame -> Anim.Frame) -> Widget f -> Widget f
atImageWithSize f = atMkUserIO g
  where
    g mkUserIO size = atUioFrame (f size) (mkUserIO size)

atImage :: (Anim.Frame -> Anim.Frame) -> Widget f -> Widget f
atImage = atUserIO . atUioFrame

userIO :: Widget f -> Size -> UserIO f
userIO = Sized.fromSize . content

image :: Widget f -> Size -> Anim.Frame
image = (fmap . fmap) uioFrame userIO

eventMap :: Widget f -> Size -> EventHandlers f
eventMap = (fmap . fmap) uioEventMap userIO

-- TODO: Would be nicer as (Direction -> Id), but then TextEdit's "f" couldn't be ((,) String)..
takesFocus :: Functor f => (Direction -> f Id) -> Widget f -> Widget f
takesFocus enter = atUserIO f
  where
    f uio = (atUioMaybeEnter . const) mEnter uio
      where
        mEnter =
          Just $
          fmap (EnterResult focalArea . eventResultFromCursor)
          enter
        focalArea = uioFocalArea uio

atMaybeEnter :: (MEnter f -> MEnter f) -> Widget f -> Widget f
atMaybeEnter = atUserIO . atUioMaybeEnter

atEventMap :: (EventHandlers f -> EventHandlers f) -> Widget f -> Widget f
atEventMap = atUserIO . atUioEventMap

-- ^ If doesn't take focus, event map is ignored
strongerEvents :: EventHandlers f -> Widget f -> Widget f
strongerEvents = atEventMap . mappend

-- ^ If doesn't take focus, event map is ignored
weakerEvents :: EventHandlers f -> Widget f -> Widget f
weakerEvents = atEventMap . flip mappend

backgroundColor :: Id -> Draw.Color -> Widget f -> Widget f
backgroundColor (Id animId) = atImageWithSize . Anim.backgroundColor animId 10

tint :: Draw.Color -> Widget f -> Widget f
tint = atImage . Anim.onImages . Draw.tint

actionEventMap ::
  Functor f => [EventMap.EventType] -> EventMap.Doc ->
  f () -> EventHandlers f
actionEventMap keys doc act =
  EventMap.fromEventTypes keys doc (emptyEventResult act)

actionEventMapMovesCursor ::
  Functor f => [EventMap.EventType] -> EventMap.Doc ->
  f Id -> EventHandlers f
actionEventMapMovesCursor keys doc act =
  EventMap.fromEventTypes keys doc $
  eventResultFromCursor act

translateUserIO :: Vector2 R -> UserIO f -> UserIO f
translateUserIO pos =
  (atUioFrame . Anim.translate) pos .
  (atUioFocalArea . Rect.atRectTopLeft) (+pos) .
  (atUioMaybeEnter . fmap . fmap . atEnterResultRect . Rect.atRectTopLeft) (+pos) .
  (atUioMaybeEnter . fmap . argument . Direction.inRelativePos . Rect.atRectTopLeft) (subtract pos)

translate :: Vector2 R -> Widget f -> Widget f
translate = atUserIO . translateUserIO

-- If widget's max size is smaller than given size, place widget in
-- portion of the extra space (0..1 ratio in each dimension):
align :: Vector2 R -> Widget f -> Widget f
align = atContent . Sized.align translateUserIO
