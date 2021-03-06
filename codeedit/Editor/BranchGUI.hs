{-# OPTIONS -O2 -Wall #-}
module Editor.BranchGUI(makeRootWidget) where

import Control.Monad (liftM, liftM2, unless)
import Data.List (findIndex)
import Data.List.Utils (removeAt)
import Data.Maybe (fromMaybe)
import Data.Monoid(Monoid(..))
import Data.Store.Rev.Branch (Branch)
import Data.Store.Rev.View (View)
import Data.Store.Transaction (Transaction)
import Editor.Anchors (ViewTag, DBTag)
import Editor.CTransaction (TWidget, runNestedCTransaction, transaction, getP, readCursor)
import Editor.MonadF (MonadF)
import qualified Data.Store.Property as Property
import qualified Data.Store.Rev.Branch as Branch
import qualified Data.Store.Rev.Version as Version
import qualified Data.Store.Rev.View as View
import qualified Data.Store.Transaction as Transaction
import qualified Editor.Anchors as Anchors
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.Config as Config
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Graphics.UI.Bottle.Widgets.Spacer as Spacer

setCurrentBranch :: Monad m => View -> Branch -> Transaction DBTag m ()
setCurrentBranch view branch = do
  Property.set Anchors.currentBranch branch
  View.setBranch view branch

deleteCurrentBranch :: Monad m => View -> Transaction DBTag m Widget.Id
deleteCurrentBranch view = do
  branch <- Property.get Anchors.currentBranch
  branches <- Property.get Anchors.branches
  let
    index =
      fromMaybe (error "Invalid current branch!") $
      findIndex ((branch ==) . snd) branches
    newBranches = removeAt index branches
  Property.set Anchors.branches newBranches
  let
    newCurrentBranch =
      newBranches !! min (length newBranches - 1) index
  setCurrentBranch view $ snd newCurrentBranch
  return . WidgetIds.fromIRef $ fst newCurrentBranch

makeBranch :: Monad m => View -> Transaction DBTag m ()
makeBranch view = do
  newBranch <- Branch.new =<< View.curVersion view
  textEditModelIRef <- Transaction.newIRef "New view"
  let viewPair = (textEditModelIRef, newBranch)
  Property.pureModify Anchors.branches (++ [viewPair])
  setCurrentBranch view newBranch

makeRootWidget :: MonadF m => TWidget ViewTag (Transaction DBTag m) -> TWidget DBTag m
makeRootWidget widget = do
  view <- getP Anchors.view
  namedBranches <- getP Anchors.branches
  viewEdit <- makeWidgetForView view widget
  currentBranch <- getP Anchors.currentBranch

  let
    makeBranchNameEdit textEditModelIRef branch = do
      branchNameEdit <-
        BWidgets.wrapDelegated FocusDelegator.NotDelegating
        (BWidgets.makeTextEdit (Transaction.fromIRef textEditModelIRef)) $
        WidgetIds.fromIRef textEditModelIRef
      return
        (branch,
         (Widget.atMaybeEnter . fmap . fmap . Widget.atEnterResultEvent)
         (setCurrentBranch view branch >>)
         branchNameEdit)
  branchNameEdits <- mapM (uncurry makeBranchNameEdit) namedBranches
  branchSelector <-
    BWidgets.makeChoice WidgetIds.branchSelection Box.vertical branchNameEdits
    currentBranch

  let
    delBranchEventMap
      | null (drop 1 namedBranches) = mempty
      | otherwise = Widget.actionEventMapMovesCursor Config.delBranchKeys "Delete Branch" $ deleteCurrentBranch view
  return .
    (Widget.strongerEvents . mconcat)
      [Widget.actionEventMap Config.quitKeys "Quit" (error "Quit")
      ,Widget.actionEventMap Config.makeBranchKeys "New Branch" $ makeBranch view
      ] .
    Box.toWidget . Box.make Box.horizontal $
    [viewEdit
    ,Widget.liftView Spacer.makeHorizontalExpanding
    ,Widget.strongerEvents delBranchEventMap branchSelector
    ]

-- Apply the transactions to the given View and convert them to
-- transactions on a DB
makeWidgetForView :: MonadF m => View -> TWidget ViewTag (Transaction DBTag m) -> TWidget DBTag m
makeWidgetForView view innerWidget = do
  curVersion <- transaction $ View.curVersion view
  curVersionData <- transaction $ Version.versionData curVersion
  redos <- getP Anchors.redos
  cursor <- readCursor

  let
    redo version newRedos = do
      Property.set Anchors.redos newRedos
      View.move view version
      Transaction.run store $ Property.get Anchors.postCursor
    undo parentVersion = do
      preCursor <- Transaction.run store $ Property.get Anchors.preCursor
      View.move view parentVersion
      Property.pureModify Anchors.redos (curVersion:)
      return preCursor

    redoEventMap [] = mempty
    redoEventMap (version:restRedos) =
      Widget.actionEventMapMovesCursor Config.redoKeys "Redo" $
      redo version restRedos
    undoEventMap =
      maybe mempty
      (Widget.actionEventMapMovesCursor Config.undoKeys "Undo" .
       undo) $ Version.parent curVersionData

    eventMap = mconcat [undoEventMap, redoEventMap redos]

    saveCursor eventResult = do
      isEmpty <- Transaction.isEmpty
      unless isEmpty $ do
        Property.set Anchors.preCursor cursor
        Property.set Anchors.postCursor . fromMaybe cursor $ Widget.eCursor eventResult
      return eventResult

  vWidget <-
    runNestedCTransaction store $
    (liftM . Widget.atEvents) (>>= saveCursor) innerWidget

  let
    lowerWTransaction act = do
      (r, isEmpty) <-
        Transaction.run store $ liftM2 (,) act Transaction.isEmpty
      unless isEmpty $ Property.set Anchors.redos []
      return r

  return .
    Widget.strongerEvents eventMap $
    Widget.atEvents lowerWTransaction vWidget
  where
    store = Anchors.viewStore view
