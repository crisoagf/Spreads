{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
module Spreads.Editor.Events where
import Spreads.Editor.State
import Spreads.Evaluate
import Spreads.Vision
import Spreads.Basic

import LensCheats

import Control.Applicative
import Control.Arrow
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Free
import Brick.Main
import Brick.Types
import Brick.Widgets.Edit
import Graphics.Vty.Input.Events
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text as T'
import Data.Text.Zipper

data Where = U | D | L | R

data EditorEventSpec a = 
  Exit                                     | 
  MovePointer Where a                      |
  ChangeColSize Int a                      |
  ChangeModeTo Mode a                      |
  FlipShowMode a                           |
  ClearEditor a                            |
  DeferToEditBar Event a                   |
  SendStatusMessage (StatusMessage Text) a |
  AddRow a                                 |
  AddColumn a                              |
  Yank a                                   |
  EmptyBarGoesToNormal a                   |
  RunCommand a                             |
  PrintCell a                              |
  RevisualiseAfter NeedsRevisual a         |
  DoIO (IOCmd a)                           deriving Functor

data RevisualCmd a =
  ChangeVisionTo VisionToken a       |
  Put a                              |
  ChangeSheetTo FullSpreadsheet a    |
  DeleteCell a                       |
  UpdateCellWithEditor a             deriving Functor

data IOCmd a =
  Save (Maybe FilePath) (Bool -> a)                  |
  Load (Maybe FilePath) (Maybe FullSpreadsheet -> a) deriving Functor

type NeedsRevisual = Free RevisualCmd ()
type NeedsIO = Free IOCmd ()

type EditorEvent = Free EditorEventSpec ()

editorEventInterpreter :: EditorEvent -> EditorState n -> EventM n (Next (EditorState n))
editorEventInterpreter (Pure ()) = continue
editorEventInterpreter (Free f) = case f of
  MovePointer d next -> editorEventInterpreter next . updateEditor . (case d of
    U -> using sheetBounds currentCell %~ (\ (bounds, CellIndex (r,c)) -> CellIndex (max r (fst (getCellIndex (fst bounds)) + 1) - 1, c))
    D -> using sheetBounds currentCell %~ (\ (bounds, CellIndex (r,c)) -> CellIndex (min r (fst (getCellIndex (snd bounds)) - 1) + 1, c))
    L -> using sheetBounds currentCell %~ (\ (bounds, CellIndex (r,c)) -> CellIndex (r, max c (snd (getCellIndex (fst bounds)) + 1) - 1))
    R -> using sheetBounds currentCell %~ (\ (bounds, CellIndex (r,c)) -> CellIndex (r, min c (snd (getCellIndex (snd bounds)) - 1) + 1)))
  ChangeModeTo   m next -> editorEventInterpreter next . updateEditorFor m . (currentMode .~ m)
  ClearEditor next -> ((commandBar . editContentsL) .~ textZipper [""] (Just 1)) >>> editorEventInterpreter next
  EmptyBarGoesToNormal next -> (\ edState -> editorEventInterpreter (if mconcat (getText $ view (commandBar . editContentsL) edState) == "" then Free (ChangeModeTo Normal next) else next) edState)
  FlipShowMode next -> (showMode %~ \case
    ShowCurrent -> ShowInput
    ShowInput -> ShowCurrent) >>> updateEditor >>> editorEventInterpreter next
  DeferToEditBar ev next -> (\ edState -> ($ edState) <$> (commandBar .~) <$> handleEditorEvent ev (view commandBar edState)) >=> editorEventInterpreter next
  AddRow next -> (sheetBounds %~ (\ (start, CellIndex (r,c)) -> (start, CellIndex (r+1,c)))) >>> editorEventInterpreter next
  AddColumn next -> (sheetBounds %~ (\ (start, CellIndex (r,c)) -> (start, CellIndex (r,c+1)))) >>> editorEventInterpreter next
  Yank next -> (using currentCell (using (cur (expressioniseCellWith VisionInput)) currentlyCopied) %~ \(a,(b,_)) -> (a,) <$> (join $ fmap (either (const Nothing) Just) b)) >>> editorEventInterpreter next
  RevisualiseAfter rev next -> revisualiseInterpreter rev >>> using currentVisionToken (using currentSheet currentSheetVisualised) %~ (\(vis, (sh, _)) -> notRelevant visualise vis sh) >>> editorEventInterpreter next
  Exit -> halt
  SendStatusMessage msg next -> (statusMessage .~ msg) >>> editorEventInterpreter next
  RunCommand next -> flip editorEventInterpreter <*> ((>> next) . commandInterpreter . T.fromStrict . mconcat . getText . view (commandBar . editContentsL))
  DoIO ioCmdNext -> liftIO . ioInterpreter ioCmdNext >=> uncurry editorEventInterpreter
  _ -> editorEventInterpreter (liftF (SendStatusMessage (Error ("Not implemented!")) ()))

revisualiseInterpreter :: NeedsRevisual -> EditorState n -> EditorState n
revisualiseInterpreter (Pure _) = id
revisualiseInterpreter (Free f) = case f of 
  UpdateCellWithEditor next -> (using currentCell (using (commandBar . editContentsL) currentSheet) %~ (\(ci, (txt', sh)) -> alterCell (const $ (\ x -> if x == "" then Nothing else Just x) $ T.fromStrict $ mconcat $ getText txt') ci sh)) >>> revisualiseInterpreter next
  ChangeVisionTo v next -> updateEditor . (currentVisionToken .~ v) >>> revisualiseInterpreter next
  Put next -> (using currentCell (using currentlyCopied (cur cell)) %~ \(ci,(a,b)) -> maybe b (Just . pretty . uncurry (shiftIndices ci)) a) >>> revisualiseInterpreter next
  DeleteCell next -> (cur cell .~ Nothing) >>> revisualiseInterpreter next
  ChangeSheetTo (newBounds, newSheet) next -> (sheetBounds .~ newBounds) . (currentSheet .~ newSheet) >>> revisualiseInterpreter next

commandInterpreter :: Text -> EditorEvent
commandInterpreter ":w" = wrap (DoIO (Save Nothing (\ b -> if b then pure () else liftF (SendStatusMessage (Error ("File saving failed!")) ()))))
commandInterpreter ":l" = wrap (DoIO (Load Nothing (maybe (liftF (SendStatusMessage (Error ("Error while loading file")) ())) ((liftF . flip RevisualiseAfter () . liftF . flip ChangeSheetTo ())))))
commandInterpreter ":q" = liftF Exit
commandInterpreter t = case T.splitAt 3 t of
  (":w ", fn) -> wrap (DoIO (Save (Just $ T.unpack fn) (\ b -> if b then pure () else liftF (SendStatusMessage (Error ("File saving failed!")) ()))))
  (":l ", fn) -> wrap (DoIO (Load (Just $ T.unpack fn) (maybe (liftF (SendStatusMessage (Error ("Error while loading file")) ())) ((liftF . flip RevisualiseAfter () . liftF . flip ChangeSheetTo ())))))
  _ -> pure ()

ioInterpreter :: IOCmd a -> EditorState n -> IO (a, EditorState n)
ioInterpreter (Save fp next) edState = fmap (uncurry (flip (,)) . fmap next) $ sequenceA (edState, save (fp <|> view currentFile edState) (((,) <$> view sheetBounds <*> view currentSheet) edState))
ioInterpreter (Load fp next) edState = fmap (uncurry (flip (,)) . fmap next) $ sequenceA (edState, load (fp <|> view currentFile edState))

updateEditor :: EditorState n -> EditorState n
updateEditor = updateEditorFor <$> view currentMode <*> id

updateEditorFor :: Mode -> EditorState n -> EditorState n
updateEditorFor Edit    = using (cur cell) (commandBar . editContentsL) %~ (\ (cellText, _) -> (maybe [""] (T'.lines . T.toStrict) cellText) & (\ lineSplit -> gotoEOL (textZipper lineSplit (Just $ length lineSplit))))
updateEditorFor Command = (commandBar . editContentsL) .~ (gotoEOL (textZipper [":"] (Just 1)))
updateEditorFor Normal  = using showMode (using (cur cell) (using (cur visualiseCell) (commandBar . editContentsL))) %~ uncurry (\case
  ShowCurrent -> (\ (_, (cellText, _)) -> gotoEOL (textZipper [maybe "" T.toStrict cellText] (Just 1)))
  ShowInput   -> (\ (cellText, (_, _)) -> gotoEOL (textZipper [maybe "" T.toStrict cellText] (Just 1))))

