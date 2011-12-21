{-# OPTIONS -Wall #-}
{-# LANGUAGE TypeOperators #-}
module Graphics.UI.Bottle.Widgets.TextEdit(Cursor, Model(..), make, makeWithLabel) where

import Data.Char (isSpace)
import Data.List (genericLength)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Data.Monoid (mconcat)
import Data.Vector.Vector2 (Vector2(..))
import Data.Record.Label ((:->), getL, setL)
import Graphics.DrawingCombinators ((%%))
import Graphics.DrawingCombinators.Utils (square, drawTextLines, textLinesWidth, textLinesHeight, textHeight)
import Graphics.UI.GLFW (
  Key(KeyBackspace, KeyDel, KeyDown, KeyEnd, KeyEnter, KeyHome,
      KeyLeft, KeyRight, KeyUp))
import Graphics.UI.Bottle.SizeRange (fixedSize)
import Graphics.UI.Bottle.Sized (Sized(..))
import Graphics.UI.Bottle.Widget (Widget(..))
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.DrawingCombinators.Affine as Affine
import qualified Graphics.UI.Bottle.EventMap as EventMap

type Cursor = Int

data Model = Model {
  modelCursor :: Cursor,
  modelText :: String
  }
  deriving (Show, Read, Eq, Ord)

splitLines :: String -> [String]
splitLines = splitOn "\n"

tillEndOfWord :: String -> String
tillEndOfWord xs = spaces ++ nonSpaces
  where
    spaces = takeWhile isSpace xs
    nonSpaces = takeWhile (not . isSpace) . dropWhile isSpace $ xs

-- | Note: maxLines prevents the *user* from exceeding it, not the
-- | given text...
make :: Draw.Font -> Int -> String -> Model -> Widget Model
make font ptSize emptyStr (Model cursor str) = Widget helper
  where
    helper hasFocus = Sized reqSize $ const (img hasFocus, Just keymap)
    displayStr = finalText str
    finalText "" = emptyStr
    finalText x  = x

    cursorWidth = 8

    reqSize = fixedSize $ Vector2 width height
    img hasFocus =
      mconcat . concat $ [
        [ Draw.translate (cursorWidth / 2, 0) %%
          drawTextLines font ptSize textLines ],
        [ cursorImage | hasFocus ]
      ]

    beforeCursor = take cursor str
    cursorPosX = textLinesWidth font ptSize . (: []) . last . splitLines $ beforeCursor
    cursorPosY = (textHeight ptSize *) . subtract 1 . genericLength . splitLines $ beforeCursor
    cursorImage =
      Draw.tint (Draw.Color 0 1 0 1) $
      Affine.translate (cursorPosX, cursorPosY) %%
      Draw.scale cursorWidth (textHeight ptSize) %%
      square

    (before, after) = splitAt cursor str
    textLength = length str
    textLines = splitLines displayStr
    width = cursorWidth + textLinesWidth font ptSize textLines
    height = textLinesHeight ptSize textLines
    lineCount = length textLines

    linesBefore = reverse (splitLines before)
    linesAfter = splitLines after
    prevLine = linesBefore !! 1
    nextLine = linesAfter !! 1
    curLineBefore = head linesBefore
    curLineAfter = head linesAfter
    cursorX = length curLineBefore
    cursorY = length linesBefore - 1

    moveAbsolute a = (max 0 . min (length str) $ a, str)
    moveRelative d = moveAbsolute (cursor + d)
    backDelete n = (cursor-n, take (cursor-n) str ++ drop cursor str)
    delete n = (cursor, before ++ drop n after)

    backDeleteWord = backDelete . length . tillEndOfWord . reverse $ before
    deleteWord = delete . length . tillEndOfWord $ after

    backMoveWord = moveRelative . negate . length . tillEndOfWord . reverse $ before
    moveWord = moveRelative . length . tillEndOfWord $ after

    singleton _doc eventType makeModel =
      EventMap.singleton eventType $
      uncurry Model . makeModel

    keys doc = mconcat . map (\event -> singleton doc event . const)

    specialKey = EventMap.KeyEventType EventMap.noMods
    ctrlSpecialKey = EventMap.KeyEventType EventMap.ctrl
    ctrlCharKey = EventMap.KeyEventType EventMap.ctrl . EventMap.charKey
    altCharKey = EventMap.KeyEventType EventMap.alt . EventMap.charKey
    homeKeys = [specialKey KeyHome, ctrlCharKey 'A']
    endKeys = [specialKey KeyEnd, ctrlCharKey 'E']

    keymap =
      mconcat . concat $ [
        [ keys "Move left" [specialKey KeyLeft] $
          moveRelative (-1)
        | cursor > 0 ],

        [ keys "Move right" [specialKey KeyRight] $
          moveRelative 1
        | cursor < textLength ],

        [ keys "Move word left" [ctrlSpecialKey KeyLeft]
          backMoveWord
        | cursor > 0 ],

        [ keys "Move word right" [ctrlSpecialKey KeyRight] moveWord
        | cursor < textLength ],

        [ keys "Move up" [specialKey KeyUp] $
          moveRelative (- cursorX - 1 - length (drop cursorX prevLine))
        | cursorY > 0 ],

        [ keys "Move down" [specialKey KeyDown] $
          moveRelative (length curLineAfter + 1 + min cursorX (length nextLine))
        | cursorY < lineCount - 1 ],

        [ keys "Move to beginning of line" homeKeys $
          moveRelative (-cursorX)
        | cursorX > 0 ],

        [ keys "Move to end of line" endKeys $
          moveRelative (length curLineAfter)
        | not . null $ curLineAfter ],

        [ keys "Move to beginning of text" homeKeys $
          moveAbsolute 0
        | cursorX == 0 && cursor > 0 ],

        [ keys "Move to end of text" endKeys $
          moveAbsolute textLength
        | null curLineAfter && cursor < textLength ],

        [ keys "Delete backwards" [specialKey KeyBackspace] $
          backDelete 1
        | cursor > 0 ],

        [ keys "Delete word backwards" [ctrlCharKey 'w']
          backDeleteWord
        | cursor > 0 ],

        let swapPoint = min (textLength - 2) (cursor - 1)
            (beforeSwap, x:y:afterSwap) = splitAt swapPoint str
            swapLetters = (min textLength (cursor + 1),
                           beforeSwap ++ y:x:afterSwap)
        in

        [ keys "Swap letters" [ctrlCharKey 't']
          swapLetters
        | cursor > 0 && textLength >= 2 ],

        [ keys "Delete forward" [specialKey KeyDel] $
          delete 1
        | cursor < textLength ],

        [ keys "Delete word forward" [altCharKey 'd']
          deleteWord
        | cursor < textLength ],

        [ keys "Delete rest of line" [ctrlCharKey 'k'] $
          delete (length curLineAfter)
        | not . null $ curLineAfter ],

        [ keys "Delete newline" [ctrlCharKey 'k'] $
          delete 1
        | null curLineAfter && cursor < textLength ],

        [ keys "Delete till beginning of line" [ctrlCharKey 'u'] $
          backDelete (length curLineBefore)
        | not . null $ curLineBefore ],

        [ singleton "Insert character" EventMap.CharEventType (insert . (: []) . fromJust . EventMap.keyEventChar) ],

        [ keys "Insert Newline" [specialKey KeyEnter] (insert "\n") ]

        ]

    insert l = (cursor', str')
      where
        cursor' = cursor + length l
        str' = concat [before, l, after]

makeWithLabel :: Draw.Font -> Int -> String -> model :-> Model -> model -> Widget model
makeWithLabel font ptSize emptyStr label model =
  fmap (flip (setL label) model) $
  make font ptSize emptyStr (getL label model)