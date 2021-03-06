{-# OPTIONS -Wall -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving, DeriveDataTypeable, DeriveFunctor #-}
module Graphics.UI.Bottle.EventMap(
  EventMap, EventType(..), Event,
  module Graphics.UI.GLFW.ModState,
  module Graphics.UI.GLFW.Events,
  lookup, singleton, fromEventType, fromEventTypes,
  Key(..), charKey, delete, Doc, eventMapDocs)
where

import Control.Monad(msum)
import Data.Char(toLower, toUpper)
import Data.List(isPrefixOf)
import Data.Map(Map)
import Data.Maybe(isJust)
import Data.Monoid(Monoid(..))
import Graphics.UI.GLFW (Key(..))
import Graphics.UI.GLFW.Events (GLFWEvent(..), KeyEvent(..), IsPress(..))
import Graphics.UI.GLFW.ModState (ModState(..), noMods, shift, ctrl, alt)
import Prelude hiding (lookup)
import qualified Data.AtFieldTH as AtFieldTH
import qualified Data.Map as Map

charKey :: Char -> Key
charKey = CharKey . toUpper

data EventType = CharEventType | KeyEventType ModState Key
  deriving (Show, Eq, Ord)

prettyKey :: Key -> String
prettyKey (CharKey x) = [toLower x]
prettyKey k
  | "Key" `isPrefixOf` show k = drop 3 $ show k
  | otherwise = show k

prettyEventType :: EventType -> String
prettyEventType CharEventType = "Character"
prettyEventType (KeyEventType ms key) =
  prettyModState ms ++ prettyKey key

prettyModState :: ModState -> String
prettyModState ms = concat $
  ["Ctrl+" | modCtrl ms] ++
  ["Alt+" | modAlt ms] ++
  ["Shift+" | modShift ms]

isCharMods :: ModState -> Bool
isCharMods ModState { modCtrl = False, modAlt = False } = True
isCharMods _ = False

type Event = KeyEvent

eventTypesOf :: Event -> [EventType]
eventTypesOf (KeyEvent Release _ _ _) = []
eventTypesOf (KeyEvent Press ms mchar k)
  | isCharMods ms && mchar == Just ' ' = [KeyEventType ms KeySpace]
  | otherwise = KeyEventType ms k : charEventType
  where
   charEventType
     | isCharMods ms && isJust mchar = [CharEventType]
     | otherwise = []

type Doc = String

data EventHandler a = EventHandler {
  ehDoc :: Doc,
  ehHandler :: Event -> a
  }
  deriving (Functor)
--AtFieldTH.make ''EventHandler

newtype EventMap a = EventMap { unEventMap :: Map EventType (EventHandler a) }
  deriving (Functor)
AtFieldTH.make ''EventMap

instance Show (EventMap a) where
  show (EventMap m) = "EventMap (keys = " ++ show (Map.keys m) ++ ")"

eventMapDocs :: EventMap a -> [(String, Doc)]
eventMapDocs =
  map f . Map.toList . unEventMap
  where
    f (eventType, eventHandler) =
      (prettyEventType eventType, ehDoc eventHandler)

filterByKey :: Ord k => (k -> Bool) -> Map k v -> Map k v
filterByKey p = Map.filterWithKey (const . p)

overrides :: EventMap a -> EventMap a -> EventMap a
EventMap x `overrides` EventMap y =
  EventMap $ x `mappend` filterByKey (`Map.notMember` x) y

instance Monoid (EventMap a) where
  mempty = EventMap mempty
  mappend = overrides

delete :: EventType -> EventMap a -> EventMap a
delete = atEventMap . Map.delete

lookup :: Event -> EventMap a -> Maybe a
lookup event eventMap =
  fmap (($ event) . ehHandler) . msum $
  map (`Map.lookup` unEventMap eventMap) (eventTypesOf event)

singleton :: EventType -> Doc -> (Event -> a) -> EventMap a
singleton eventType doc handler =
  EventMap . Map.singleton eventType $
  EventHandler {
    ehDoc = doc,
    ehHandler = handler
    }

fromEventType :: EventType -> Doc -> a -> EventMap a
fromEventType eventType doc = singleton eventType doc . const

fromEventTypes :: [EventType] -> Doc -> a -> EventMap a
fromEventTypes keys doc act =
  mconcat $ map (flip (`fromEventType` doc) act) keys
