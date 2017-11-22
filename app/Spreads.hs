{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
module Main (main, exampleSheetTypes, exampleSheetEval) where
import Spreads.Basic
import Spreads.Vision
import Spreads.Editor.Events
import Spreads.Editor.State
import Spreads.Evaluate

import Spread hiding (at)
import Control.Applicative

import Control.Arrow hiding (first, second, (<+>))
import Data.Ix (range)
import Control.Lens
import Data.Void
import Control.Monad
import Control.Monad.Free

import Data.Maybe
import Data.Monoid ((<>))

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text as T'

import Brick
import Brick.Widgets.Edit
import Brick.Widgets.Border
import Graphics.Vty.Attributes
import Graphics.Vty.Input.Events

exampleSheet :: Spreadsheet Text
exampleSheet = fromList (zip (range (CellIndex (0,0), CellIndex (2,4)))
  [ "1", "2", "3.5", "#Int", "[6,7,8:#Int]"
  , "4", "2017-02-02 03:24:12.2313", "@!{1,2}", "Plus 8 5", "\"1\""
  , "\\x:#Pos -> (Sum (Range #Int @{0,0} x))", "@!{2,0} @{1,0}", "\\a:* -> \\b:* -> \\x:a -> \\y:b -> x", "\\a:* -> \\b:* -> \\c:* -> \\x:(Π _ : a -> Π _ : b -> c) -> \\y:(Π _ : a -> b) -> \\z:a -> (x z (y z))", "(\\a:* -> (\\b:* -> (@!{2,3} a (Π_:b -> a) a (@!{2,2} a (Π_:b -> a)) (@!{2,2} a b)))) #Int #Int 0"])

exampleSheetVals :: Spreadsheet (Either CellError CellExpr)
exampleSheetVals = spreadsheetParse exampleSheet

exampleSheetTypes :: Spreadsheet (Either CellError CellType)
exampleSheetTypes = spreadsheetInferTypes stdLib exampleSheetVals

exampleSheetEval :: Spreadsheet (Either CellError CellWithType)
exampleSheetEval = spreadsheetEval stdLib exampleSheetVals

defaultCellConfig :: CellConfig
defaultCellConfig = CellConfig 1 27 0 0 -- (min h ((1+) $ fromIntegral $ uncurry (flip (-) `on` fst) $ bounds exampleSheet)) (fromIntegral $ min 15 ((1+) $ uncurry (flip (-) `on` snd) $ bounds exampleSheet))

main :: IO ()
main = void $ defaultMain mainApp $ EditorState 
  exampleSheet
  (notRelevant visualise Value exampleSheet)
  (CellIndex (0,0), CellIndex (2,4))
  (CellIndex (0,0))
  0
  0
  defaultCellConfig
  Value
  Normal
  ShowInput
  (teditor (maybe "" id (visualise VisionValue exampleSheet ^. at (CellIndex (0,0)))))
  (Info "")
  Nothing
  Nothing

mainApp :: App (EditorState Text) Void Text
mainApp = App drawUi cursorChoose eventHandler startHook setAttrs

cursorChoose :: EditorState n -> [a] -> Maybe a
cursorChoose = view currentMode >>> (\case
  Normal -> const Nothing
  Edit -> listToMaybe
  Command -> listToMaybe)

startHook :: EditorState n -> EventM a (EditorState n)
startHook = pure

setAttrs :: EditorState n -> AttrMap
setAttrs _ = attrMap defAttr [("cell" <> "selected", black `on` white), ("status", black `on` white), ("status" <> "error", white `on` red)]

eventHandler :: EditorState n -> BrickEvent n Void -> EventM n (Next (EditorState n))
eventHandler edState ev = (view currentMode <**> flip (\case
  Normal -> editorEventInterpreter (case ev of 
    VtyEvent (EvKey (KChar 'p') []) -> liftF (RevisualiseAfter (liftF (Put ())) ()) >> liftF (MovePointer D ())
    VtyEvent (EvKey (KChar 'y') []) -> liftF (Yank ())
    VtyEvent (EvKey KLeft  [])      -> liftF (MovePointer L ())
    VtyEvent (EvKey KRight [])      -> liftF (MovePointer R ())
    VtyEvent (EvKey KUp    [])      -> liftF (MovePointer U ())
    VtyEvent (EvKey KDown  [])      -> liftF (MovePointer D ())
    VtyEvent (EvKey (KChar 's') []) -> liftF (FlipShowMode   ())
    VtyEvent (EvKey (KChar 'c') []) -> liftF (ChangeModeTo   Edit ()) >> liftF (ClearEditor ())
    VtyEvent (EvKey (KChar 'e') []) -> liftF (ChangeModeTo   Edit ())
    VtyEvent (EvKey (KChar 'v') []) -> liftF (RevisualiseAfter (liftF (ChangeVisionTo Value ())) ())
    VtyEvent (EvKey (KChar 'i') []) -> liftF (RevisualiseAfter (liftF (ChangeVisionTo Input ())) ())
    VtyEvent (EvKey (KChar 't') []) -> liftF (RevisualiseAfter (liftF (ChangeVisionTo Type  ())) ())
    VtyEvent (EvKey (KChar 'd') []) -> liftF (RevisualiseAfter (liftF (DeleteCell ())) ())
    VtyEvent (EvKey (KChar '+') []) -> liftF (AddRow ())
    VtyEvent (EvKey (KChar '*') []) -> liftF (AddColumn ())
    VtyEvent (EvKey (KChar ':') []) -> liftF (ChangeModeTo Command ())
    _                               -> pure ())
  Edit -> editorEventInterpreter (case ev of
    VtyEvent (EvKey KEsc   []) -> liftF $ ChangeModeTo Normal ()
    VtyEvent (EvKey KEnter []) -> liftF (RevisualiseAfter (liftF (UpdateCellWithEditor ())) ()) >> liftF (ChangeModeTo Normal ())
    VtyEvent ev'                -> liftF $ DeferToEditBar ev' ()
    _ -> pure ())
  Command -> editorEventInterpreter (case ev of
    VtyEvent (EvKey KEsc   []) -> liftF $ ChangeModeTo Normal ()
    VtyEvent (EvKey KEnter []) -> liftF (RunCommand ()) >> liftF (ChangeModeTo Normal ())
    VtyEvent ev'                -> liftF (DeferToEditBar ev' ()) >> liftF (EmptyBarGoesToNormal ())
    _ -> pure ()))) edState


teditor :: Text -> Editor T'.Text Text
teditor t = editorText "commandBar" (Just 1) (T.toStrict t)

drawUi :: EditorState Text -> [Widget Text]
drawUi edState = [padBottom Max (viewport "spreadsheet" Both (drawSpreadsheet edState)) <=> hBorder <=> renderEditor (\ [x] -> txt x) True (view commandBar edState)]

drawSpreadsheet :: EditorState n -> Widget n
drawSpreadsheet edState = 
  uncurry (<=>) $ foldr (\ pos@(CellIndex (_,j)) (lineSoFar, p) -> 
    if j == 0
    then (emptyWidget, (drawCell edState pos (view (visualiseCell pos) edState) <+> lineSoFar) <=> p)
    else (drawCell edState pos (view (visualiseCell pos) edState) <+> lineSoFar, p)) (emptyWidget, emptyWidget) (range $ view sheetBounds edState)

drawCell :: EditorState a -> CellIndex -> Maybe Text -> Widget b
drawCell edState pos t' =
  let t = maybe (T'.replicate cw " ") T.toStrict t'
      cw = fromIntegral (view (cellConfig . cellWidth) edState) in (if view currentCell edState == pos then visible . withAttr ("cell" <> "selected") else withAttr ("cell" <> "default")) $ if textWidth t <= cw then padRight (Pad (fromIntegral $ cw - textWidth t)) (txt t) else (txt (T'.take (cw - 3) t <> "..."))

