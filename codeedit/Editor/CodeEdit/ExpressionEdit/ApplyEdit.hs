{-# LANGUAGE OverloadedStrings #-}
module Editor.CodeEdit.ExpressionEdit.ApplyEdit(make) where

import Control.Monad (liftM)
import Data.Maybe (fromMaybe)
import Editor.Anchors (ViewTag)
import Editor.CTransaction (TWidget, getP, assignCursor, transaction)
import Editor.CodeEdit.Ancestry (AncestryItem(..), ApplyParent(..))
import Editor.CodeEdit.ExpressionEdit.ExpressionMaker (ExpressionEditMaker)
import Editor.DataOps (ExpressionPtr)
import Editor.MonadF (MonadF)
import qualified Data.Store.Transaction as Transaction
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.CodeEdit.Ancestry as Ancestry
import qualified Editor.CodeEdit.Infix as Infix
import qualified Editor.CodeEdit.Sugar as Sugar
import qualified Editor.Config as Config
import qualified Editor.Data as Data
import qualified Editor.DataOps as DataOps
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.UI.Bottle.Widget as Widget

make
  :: MonadF m
  => ExpressionEditMaker m
  -> Ancestry.ExpressionAncestry m
  -> ExpressionPtr m
  -> Sugar.Apply m
  -> Widget.Id
  -> TWidget ViewTag m
make makeExpressionEdit ancestry expressionPtr (Sugar.Apply func arg) myId = do
  expressionI <- getP expressionPtr
  -- TODO: Remove this when obliterate ancestry
  Data.ExpressionApply origApply <- transaction $ Transaction.readIRef expressionI
  argI <- getP $ Sugar.rExpressionPtr arg
  assignCursor myId (WidgetIds.fromIRef argI) $ do
    funcI <- getP $ Sugar.rExpressionPtr func
    -- TODO: This will come from sugar
    isInfix <- transaction $ Infix.isInfixFunc funcI
    isApplyOfInfix <- transaction $ Infix.isApplyOfInfixOp funcI
    mInfixOpOfRArg <- transaction $ Infix.infixFuncOfRArg funcI
    let
      funcType
        | isInfix = Ancestry.InfixLeft
        | isApplyOfInfix = Ancestry.InfixRight
        | otherwise = Ancestry.Prefix
      delArgTarget = fromMaybe funcI mInfixOpOfRArg
      addDelEventMap target =
        liftM . Widget.weakerEvents .
        Widget.actionEventMapMovesCursor Config.delKeys "Delete" .
        liftM WidgetIds.fromIRef .
        (>> return target) . DataOps.replace expressionPtr

    let
      makeAncestry role =
        AncestryItemApply (ApplyParent role funcType origApply expressionPtr) : ancestry
    funcEdit <-
      addDelEventMap argI argI $
      makeExpressionEdit (makeAncestry Ancestry.ApplyFunc) func

    argEdit <-
      addDelEventMap delArgTarget funcI $
      makeExpressionEdit (makeAncestry Ancestry.ApplyArg) arg

    return . BWidgets.hbox $
      (if isInfix then reverse else id)
      [funcEdit, BWidgets.spaceWidget, argEdit]
