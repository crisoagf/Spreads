{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module Spread where
import Spread.StdLib.Int
import Spread.StdLib.Range
import Spread.TypesAndVals
import Spread.Typecheck
import Spread.Parser

import Control.Arrow hiding (first, second)
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader

import Data.Array
import Data.Bifunctor
import Data.Function
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid ((<>))

import Data.Text.Buildable
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO

import System.Console.Haskeline

import Numeric.Natural
import UI.NCurses
import Morte.Core
import Text.Parsec

instance MonadException Curses where
  controlIO act = join $ liftIO $ act $ RunIO pure

data Vision = Input | Type | Value | Presentation
data Mode = Normal | Edit CellIndex
data EditorState = EditorState { _currentSheet :: Array CellIndex Text, _currentCell :: CellIndex, _selColor :: ColorID, _norColor :: ColorID, _windowHeight :: Integer, _windowWidth :: Integer, _cellConfig :: CellConfig, _currentVision :: Vision, _currentMode :: Mode, _statusBar :: Text }
data CellConfig = CellConfig { _cellHeight :: Integer, _cellWidth :: Integer, _numVisRows :: Integer, _numVisCols :: Integer} deriving Eq

joinLenses :: Lens s s a a -> Lens s s b b -> Lens s s (a, b) (a, b)
joinLenses la lb = lens (\ s -> (view la s, view lb s)) (\ s (a,b) -> (s & la .~ a) & lb .~ b)

makeLenses ''EditorState
makeLenses ''CellConfig

spreadsheetParse :: Array CellIndex Text -> Array CellIndex (Either ParseError CellExpr)
spreadsheetParse = fmap (parse parseCell "input")

spreadsheetInferTypes :: (NamedReference -> Maybe CellRawExpr) -> Array CellIndex CellExpr -> Array CellIndex (Either TypeError CellType)
spreadsheetInferTypes refMap arr = fmap (join . fmap ((`runReader` (InferContext (either (fmap snd . getType) (ExceptT . pure . fmap snd . (go !))) refMap)) . runExceptT . uncurry returnTypes)) $ go where go = fmap ((`runReader` (InferContext (either (fmap snd . getType) (ExceptT . pure . fmap snd . (go !))) refMap)) . runExceptT . getType) arr

spreadsheetEval :: (NamedReference -> Maybe CellRawExpr) -> Array CellIndex CellExpr -> Array CellIndex (Either TypeError CellWithType)
spreadsheetEval refMap arr = go where go = fmap ((`runReader` (EvalContext (either typeAndEvaluate (ExceptT . pure . (go !))) refMap)) . runExceptT . typeAndEvaluate) arr

exampleSheet :: Array (Natural, Natural) Text
exampleSheet = listArray ((0,0),(2,4))
  [ "1", "2", "3.5", "#Int", "[6,7,8:Int]"
  , "4", "5", "@{1,2}", "Plus 8 5", "\"1\""
  , "\\x:Pos => (Sum (Range #Int @{0,0} x))", "@!{2,0} @{1,1}", "\\a:* => \\b:* => \\x:a => \\y:b => x", "\\a:* => \\b:* => \\c:* => \\x:(a -> b -> c) => \\y:(a -> b) => \\z:a => (x z (y z))", "\\a:* => (\\b:* => (@!{2,3} a #(b -> a) a (@!{2,2} a #(b -> a)) (@!{2,2} a b)))"]

exampleSheetVals :: Either ParseError (Array CellIndex CellExpr)
exampleSheetVals = traverse (parse parseCell "input") exampleSheet

exampleSheetTypes :: Either (Either ParseError TypeError) (Array CellIndex CellType)
exampleSheetTypes = either (Left . Left) (first Right) $ (sequence . spreadsheetInferTypes stdLib) <$> exampleSheetVals

exampleSheetEval :: Either (Either ParseError TypeError) (Array CellIndex CellWithType)
exampleSheetEval = either (Left . Left) (first Right) $ (sequence . spreadsheetEval stdLib) <$> exampleSheetVals

stdLib :: NamedReference -> Maybe CellRawExpr
stdLib = flip lookup (basicIntFns ++ basicRangeFns)

main :: IO ()
main = do
  runCurses $ do
    win <- defaultWindow
    (h, w) <- updateWindow win windowSize
    normalColor <- newColorID ColorWhite ColorDefault 1
    selectedColor <- newColorID ColorBlack ColorWhite 2
    let defaultCellConfig = CellConfig 1 (w `div` 15) (min h ((1+) $ fromIntegral $ uncurry (flip (-) `on` fst) $ bounds exampleSheet)) (fromIntegral $ min 15 ((1+) $ uncurry (flip (-) `on` snd) $ bounds exampleSheet))
    let defaultEdState = EditorState exampleSheet (0,0) selectedColor normalColor h w defaultCellConfig Input Normal (exampleSheet ! (0,0))
    setEditorState win defaultEdState 
    fix (\ loop edState -> do
      ev <- getEvent win Nothing
      let (newEdState', exit) = modeInterpreter edState (_currentMode edState) ev
      when (not exit) $ do
        when (view cellConfig edState /= view cellConfig newEdState') (updateWindow win clear)
        let newEdState = newEdState' { _statusBar = (uncurry (!) (newEdState' ^. joinLenses currentSheet currentCell))}
        setEditorState win newEdState
        loop newEdState) defaultEdState

setEditorState :: Window -> EditorState -> Curses ()
setEditorState win edState = updateWindow win (showSheet edState >> updateStatusBar edState) >> render

modeInterpreter :: EditorState -> Mode -> Maybe Event -> (EditorState, Bool)
modeInterpreter edState Normal ev =
  let w = _windowWidth edState in case ev of
        Just (EventCharacter 'q') -> (edState, True)
        Just (EventCharacter 'i') -> (edState & currentVision .~ Input, False)
        Just (EventCharacter 't') -> (edState & currentVision .~ Type , False)
        Just (EventCharacter 'v') -> (edState & currentVision .~ Value, False)
        Just (EventCharacter '+') -> (edState & joinLenses (cellConfig . numVisCols) (cellConfig . cellWidth) %~ (\ (visCols, cw) -> (visCols, min (w `div` visCols) (cw + 1))), False)
        Just (EventCharacter '-') -> (edState & (cellConfig . cellWidth) %~ (\ cw -> max 1 (cw - 1)), False)
        Just (EventSpecialKey KeyLeftArrow)  -> (edState & joinLenses currentSheet currentCell %~ (\ (exampleSheet, (r,c)) -> (exampleSheet, (r, max c (snd (fst $ bounds exampleSheet) + 1) - 1))), False)
        Just (EventSpecialKey KeyRightArrow) -> (edState & joinLenses currentSheet currentCell %~ (\ (exampleSheet, (r,c)) -> (exampleSheet, (r, min c (snd (snd $ bounds exampleSheet) - 1) + 1))), False)
        Just (EventSpecialKey KeyUpArrow)    -> (edState & joinLenses currentSheet currentCell %~ (\ (exampleSheet, (r,c)) -> (exampleSheet, (max r (fst (fst $ bounds exampleSheet) + 1) - 1, c))), False)
        Just (EventSpecialKey KeyDownArrow)  -> (edState & joinLenses currentSheet currentCell %~ (\ (exampleSheet, (r,c)) -> (exampleSheet, (min r (fst (snd $ bounds exampleSheet) - 1) + 1, c))), False)
        _ -> (edState, False)

showSheet :: EditorState -> Update ()
showSheet edState =
  let s      = _currentSheet edState
      selInd = _currentCell edState
      sel    = _selColor edState
      nor    = _norColor edState
      h      = _windowHeight edState
      w      = _windowWidth edState
      ch     = min (_cellHeight (_cellConfig edState)) (h `div` _numVisRows (_cellConfig edState))
      cw     = min (_cellWidth  (_cellConfig edState)) ((w - 1) `div` _numVisCols (_cellConfig edState)) in case _currentVision edState of
        Input -> forM_ (assocs $ fmap (parse parseCell "input") s) (showCell ch cw selInd nor sel)
        Type -> case sequence (spreadsheetParse s) of
          Left pe -> moveCursor h 0 >> drawText "Please solve all parsing errors before typing"
          Right sheet -> forM_ (assocs $ spreadsheetInferTypes stdLib sheet) (showCell ch cw selInd nor sel)
        Value -> case sequence (spreadsheetParse s) of
          Left pe -> moveCursor h 0 >> drawText "Please solve all parsing errors before evaluating"
          Right sheet -> forM_ (assocs $ fmap (fmap cellValue) $ spreadsheetEval stdLib sheet) (showCell ch cw selInd nor sel)

showCell :: (Show e, Buildable s) => Integer -> Integer -> CellIndex -> ColorID -> ColorID -> (CellIndex, Either e s) -> Update ()
showCell ch cw (sx, sy) nor sel ((i,j), t) = do
  moveCursor (fromIntegral i * ch) (fromIntegral j * cw)
  drawString (replicate (fromIntegral $ cw - 1) ' ')
  moveCursor (fromIntegral i * ch) (fromIntegral j * cw)
  when (sx == i && sy == j) $ setColor sel
  let t' = padAndTruncate (cw - 1) $ either (T.pack . show) pretty t
  drawText (T.toStrict t')
  when (sx == i && sy == j) $ setColor nor

updateStatusBar :: EditorState -> Update ()
updateStatusBar edState = do
  moveCursor (_windowHeight edState - 1) 0
  clearLine
  drawText (T.toStrict $ _statusBar edState)

padAndTruncate :: Integral i => i -> Text -> Text
padAndTruncate i t = if T.length t <= fromIntegral i then t <> T.replicate (fromIntegral i - T.length t) " " else T.take (fromIntegral i - 3) t <> "..."

