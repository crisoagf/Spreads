{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
module Main where
import Spread.StdLib.Int
import Spread.StdLib.Range
import Spread.TypesAndVals
import Spread.Typecheck
import Spread.Parser

import Control.Arrow hiding (first, second, (<+>))
import Control.Lens
import Control.Monad
import Control.Monad.Free
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader

import Data.Array
import Data.Bifunctor
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid ((<>))

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO
import qualified Data.Text as T'
import Data.Text.Zipper

import Numeric.Natural
import Brick
import Brick.Widgets.Core
import Brick.Widgets.Center
import Brick.Widgets.Edit
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Graphics.Vty.Attributes
import Graphics.Vty.Input.Events
import Morte.Core hiding (App)
import qualified Morte.Core as M

data Vision   = Input | Type | Value | Presentation
data Mode     = Normal | Edit
data ShowMode = ShowInput | ShowCurrent
data EditorState = EditorState { _currentSheet :: Array CellIndex Text, _currentCell :: CellIndex, _windowHeight :: Integer, _windowWidth :: Integer, _cellConfig :: CellConfig, _currentVision :: Vision, _currentMode :: Mode, _showMode :: ShowMode, _commandBar :: Editor T'.Text () }
data CellConfig = CellConfig { _cellHeight :: Integer, _cellWidth :: Integer, _numVisRows :: Integer, _numVisCols :: Integer} deriving Eq

data EditorEventSpec a = 
  Exit | 
  MovePointer Where a |
  ChangeColSize Int a |
  ChangeModeTo Mode a |
  ChangeVisionTo Vision a |
  UpdateCellWithEditor a | 
  FlipShowMode a |
  PrintCell a deriving Functor

type EditorEvent = Free EditorEventSpec ()

data Where = U | D | L | R

joinLenses :: Lens s s a a -> Lens s s b b -> Lens s s (a, b) (a, b)
joinLenses la lb = lens (\ s -> (view la s, view lb s)) (\ s (a,b) -> (s & la .~ a) & lb .~ b)

makeLenses ''EditorState
makeLenses ''CellConfig

spreadsheetParse :: Array CellIndex (Either CellError Text) -> Array CellIndex (Either CellError CellExpr)
spreadsheetParse = fmap (exprFromText "input" =<<)

spreadsheetInferTypes :: (NamedReference -> Maybe CellRawExpr) -> Array CellIndex (Either CellError CellExpr) -> Array CellIndex (Either CellError CellType)
spreadsheetInferTypes refMap arr = go where go = fmap (((`runReader` (InferContext (either getType (ExceptT . pure . (go !))) refMap)) . runExceptT . getType) =<<) arr

spreadsheetEval :: (NamedReference -> Maybe CellRawExpr) -> Array CellIndex (Either CellError CellExpr) -> Array CellIndex (Either CellError CellWithType)
spreadsheetEval refMap arr = go where go = fmap (((`runReader` (EvalContext (either typeAndEvaluate (ExceptT . pure . (go !))) refMap)) . runExceptT . typeAndEvaluate) =<<) arr

exampleSheet :: Array (Natural, Natural) Text
exampleSheet = listArray ((0,0),(2,4))
  [ "1", "2", "3.5", "#Int", "[6,7,8:Int]"
  , "4", "5", "@{1,2}", "Plus 8 5", "\"1\""
  , "\\x:Pos => (Sum (Range #Int @{0,0} x))", "@!{2,0} @{1,1}", "\\a:* => \\b:* => \\x:a => \\y:b => x", "\\a:* => \\b:* => \\c:* => \\x:(a -> b -> c) => \\y:(a -> b) => \\z:a => (x z (y z))", "(\\a:* => (\\b:* => (@!{2,3} a #(b -> a) a (@!{2,2} a #(b -> a)) (@!{2,2} a b)))) #Int #Int 0"]

exampleSheetVals :: Array CellIndex (Either CellError CellExpr)
exampleSheetVals = spreadsheetParse $ fmap pure exampleSheet

exampleSheetTypes :: Array CellIndex (Either CellError CellType)
exampleSheetTypes = spreadsheetInferTypes stdLib exampleSheetVals

exampleSheetEval :: Array CellIndex (Either CellError CellWithType)
exampleSheetEval = spreadsheetEval stdLib exampleSheetVals

stdLib :: NamedReference -> Maybe CellRawExpr
stdLib = flip lookup (basicIntFns ++ basicRangeFns)

defaultCellConfig = CellConfig 1 27 0 0 -- (min h ((1+) $ fromIntegral $ uncurry (flip (-) `on` fst) $ bounds exampleSheet)) (fromIntegral $ min 15 ((1+) $ uncurry (flip (-) `on` snd) $ bounds exampleSheet))

main :: IO ()
main = void $ defaultMain mainApp $ EditorState exampleSheet (0,0) 0 0 defaultCellConfig Value Normal ShowCurrent (teditor (visualize Input exampleSheet ! (0,0)))

mainApp :: App EditorState EditorEvent ()
mainApp = App drawUi cursorChoose eventHandler startHook setAttrs

cursorChoose edState [inputBar] = case _currentMode edState of
  Normal -> Nothing
  Edit -> Just inputBar

startHook :: EditorState -> EventM a EditorState
startHook = pure

setAttrs :: EditorState -> AttrMap
setAttrs _ = attrMap defAttr [("cell" <> "selected", black `on` white)]

eventHandler :: EditorState -> BrickEvent () EditorEvent -> EventM () (Next EditorState)
eventHandler edState ev = case _currentMode edState of
  Normal -> edState & editorEventInterpreter (case ev of 
    VtyEvent (EvKey (KChar 'q') []) -> liftF Exit
    VtyEvent (EvKey (KChar 'p') []) -> liftF (PrintCell ())
    VtyEvent (EvKey KLeft  [])      -> liftF (MovePointer L ())
    VtyEvent (EvKey KRight [])      -> liftF (MovePointer R ())
    VtyEvent (EvKey KUp    [])      -> liftF (MovePointer U ())
    VtyEvent (EvKey KDown  [])      -> liftF (MovePointer D ())
    VtyEvent (EvKey (KChar 's') []) -> liftF (FlipShowMode   ())
    VtyEvent (EvKey (KChar 'e') []) -> liftF (ChangeModeTo   Edit ())
    VtyEvent (EvKey (KChar 'v') []) -> liftF (ChangeVisionTo Value ())
    VtyEvent (EvKey (KChar 'i') []) -> liftF (ChangeVisionTo Input ())
    VtyEvent (EvKey (KChar 't') []) -> liftF (ChangeVisionTo Type  ())
    _                               -> pure ())
  Edit -> case ev of
    VtyEvent (EvKey KEsc   []) -> editorEventInterpreter (liftF $ ChangeModeTo Normal ()) edState
    VtyEvent (EvKey KEnter []) -> editorEventInterpreter (liftF (UpdateCellWithEditor ()) >> liftF (ChangeModeTo Normal ())) edState
    VtyEvent ev -> continue =<< ((\ed -> edState & commandBar .~ ed) <$> handleEditorEvent ev (_commandBar edState))
    _ -> continue edState

editorEventInterpreter :: EditorEvent -> EditorState -> EventM () (Next EditorState)
editorEventInterpreter (Pure ()) = continue
editorEventInterpreter (Free f) = case f of
  MovePointer d next -> editorEventInterpreter next . updateEditorText . (case d of
    U -> joinLenses currentSheet currentCell %~ (\ (exampleSheet, (r,c)) -> (exampleSheet, (max r (fst (fst $ bounds exampleSheet) + 1) - 1, c)))
    D -> joinLenses currentSheet currentCell %~ (\ (exampleSheet, (r,c)) -> (exampleSheet, (min r (fst (snd $ bounds exampleSheet) - 1) + 1, c)))
    L -> joinLenses currentSheet currentCell %~ (\ (exampleSheet, (r,c)) -> (exampleSheet, (r, max c (snd (fst $ bounds exampleSheet) + 1) - 1)))
    R -> joinLenses currentSheet currentCell %~ (\ (exampleSheet, (r,c)) -> (exampleSheet, (r, min c (snd (snd $ bounds exampleSheet) - 1) + 1))))
  ChangeVisionTo v next -> editorEventInterpreter next . updateEditorText . (currentVision .~ v)
  ChangeModeTo   Edit next -> editorEventInterpreter next . updateEditorForEdit . (currentMode .~ Edit)
  ChangeModeTo   m next -> editorEventInterpreter next . updateEditorText . (currentMode .~ m)
  UpdateCellWithEditor next -> editorEventInterpreter next . (joinLenses (joinLenses currentCell (commandBar . editContentsL)) currentSheet %~ (\((ci, txt), sh) -> ((ci,txt), sh // [(ci, T.fromStrict $ mconcat $ getText txt)])))
  PrintCell next -> editorEventInterpreter next . (joinLenses (joinLenses currentCell (commandBar . editContentsL)) currentSheet %~ (\((ci, txt), sh) -> ((ci,txt), sh // [(ci, T.fromStrict $ mconcat $ getText txt)])))
  FlipShowMode next -> editorEventInterpreter next . updateEditorText . (showMode %~ \case
    ShowCurrent -> ShowInput
    ShowInput -> ShowCurrent)
  Exit -> halt

updateEditorText :: EditorState -> EditorState 
updateEditorText edState = case _showMode edState of
  ShowCurrent -> edState { _commandBar = applyEdit (const $ textZipper [T.toStrict $ visualize (_currentVision edState) (_currentSheet edState) ! (_currentCell edState)] (Just 1)) $ _commandBar edState}
  ShowInput -> edState { _commandBar = applyEdit (const $ textZipper [T.toStrict $ _currentSheet edState ! _currentCell edState] (Just 1)) $ _commandBar edState}

updateEditorForEdit :: EditorState -> EditorState 
updateEditorForEdit edState = edState { _commandBar = applyEdit (const $ textZipper [T.toStrict $ _currentSheet edState ! _currentCell edState] (Just 1)) $ _commandBar edState}

teditor :: Text -> Editor T'.Text ()
teditor t = editorText () (Just 1) (T.toStrict t)

drawUi :: EditorState -> [Widget ()]
drawUi edState = [padBottom Max (drawSpreadsheet edState) <=> hBorder <=> renderEditor (\ [x] -> txt x) True (_commandBar edState)]

drawSpreadsheet :: EditorState -> Widget ()
drawSpreadsheet edState =
  let sh = _currentSheet edState 
      cellInd = _currentCell edState
      vis = _currentVision edState
      ccfg = _cellConfig edState in
        uncurry (<=>) $ foldr (\ (pos@(i,j),t) (lineSoFar, p) -> (if j == 0 then (emptyWidget, (drawCell pos cellInd ccfg t <+> lineSoFar) <=> p) else (drawCell pos cellInd ccfg t <+> lineSoFar, p))) (emptyWidget, emptyWidget) (assocs $ visualize vis sh)

drawCell :: CellIndex -> CellIndex -> CellConfig -> Text -> Widget b
drawCell pos cellInd (CellConfig _ cw _ _) t = withAttr (if pos == cellInd then "cell" <> "selected" else "cell" <> "default") $ if T.length t < fromIntegral cw then padRight (Pad (fromIntegral $ fromIntegral cw - T.length t)) (txt (T.toStrict $ t)) else (txt (T.toStrict $ T.take (fromIntegral cw - 3) t <> "..."))

visualize :: Vision -> Array CellIndex Text -> Array CellIndex Text
visualize Input = fmap pretty . spreadsheetParse . fmap Right
visualize Type = fmap pretty . spreadsheetInferTypes stdLib . spreadsheetParse . fmap Right
visualize Value = fmap (pretty . fmap cellValue) . spreadsheetEval stdLib . spreadsheetParse . fmap Right

