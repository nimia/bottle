module Editor.CodeEdit.ExpressionEdit.WhereEdit(make) where

import Control.Monad (liftM)
import Data.Vector.Vector2 (Vector2(..))
import Editor.Anchors (ViewTag)
import Editor.CTransaction (TWidget, atTextSizeColor)
import Editor.MonadF (MonadF)
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.CodeEdit.ParamEdit as ParamEdit
import qualified Editor.CodeEdit.Sugar as Sugar
import qualified Editor.CodeEdit.Types as ETypes
import qualified Editor.Config as Config
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Grid as Grid

make
  :: MonadF m
  => ETypes.ExpressionEditMaker m
  -> ETypes.ExpressionAncestry m
  -> Sugar.Where m
  -> Widget.Id -> TWidget ViewTag m
make makeExpressionEdit ancestry w@(Sugar.Where items bodyI) myId = do
  whereLabel <-
    atTextSizeColor Config.whereTextSize Config.whereColor $
    BWidgets.makeLabel "where" myId
  bodyEdit <- makeExpressionEdit bodyAncestry bodyI
  whereEdits <- mapM makeWhereItemEdits items
  return . BWidgets.vbox $ [
    bodyEdit,
    whereLabel,
    Grid.toWidget $ Grid.make whereEdits
    ]
  where
    makeAncestry role = ETypes.AncestryItemWhere (ETypes.WhereParent w role) : ancestry
    bodyAncestry = makeAncestry ETypes.WhereBody
    witemAncestry = makeAncestry . ETypes.WhereDef
    makeWhereItemEdits item =
      (mapM . liftM . Widget.weakerEvents) (whereItemDeleteEventMap item)
      [ (liftM . Widget.align) (Vector2 1 0.5) . ParamEdit.make $ Sugar.wiParamI item
      , (liftM . Widget.align) (Vector2 0.5 0.5) . BWidgets.makeLabel "=" . WidgetIds.fromIRef $ Sugar.wiParamI item
      , (liftM . Widget.align) (Vector2 0 0.5) . makeExpressionEdit (witemAncestry (Sugar.wiParamI item)) $ Sugar.wiExprPtr item
      ]
    whereItemDeleteEventMap item =
      Widget.actionEventMapMovesCursor Config.delKeys "Delete variable" .
      liftM WidgetIds.fromIRef $ Sugar.wiRemoveItem item