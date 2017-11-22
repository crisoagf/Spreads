{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module Spreads.Editor.State where
import Spreads.Vision
import Spreads.Basic
import Brick.Widgets.Edit
import Data.Text.Lazy (Text)
import qualified Data.Text as T'
import Control.Lens

data Mode     = Normal | Edit | Command
data ShowMode = ShowInput | ShowCurrent
data EditorState n = 
  EditorState {
    _currentSheet :: Spreadsheet Text,
    _currentSheetVisualised :: Spreadsheet Text,
    _sheetBounds :: (CellIndex, CellIndex),
    _currentCell :: CellIndex,
    _windowHeight :: Integer,
    _windowWidth :: Integer,
    _cellConfig :: CellConfig,
    _currentVisionToken :: VisionToken,
    _currentMode :: Mode,
    _showMode :: ShowMode,
    _commandBar :: Editor T'.Text n,
    _statusMessage :: StatusMessage Text,
    _currentlyCopied :: Maybe (CellIndex, CellExpr),
    _currentFile :: Maybe FilePath }
data CellConfig =
  CellConfig {
    _cellHeight :: Integer,
    _cellWidth  :: Integer,
    _numVisRows :: Integer,
    _numVisCols :: Integer} deriving Eq

data StatusMessage a = Error a | Warning a | Info a deriving Show

makeLenses ''EditorState
makeLenses ''CellConfig

cell :: CellIndex -> Lens (EditorState a) (EditorState a) (Maybe Text) (Maybe Text)
cell x = currentSheet . at x

visualiseCell :: CellIndex -> Getter (EditorState a) (Maybe Text)
visualiseCell x = currentSheetVisualised . at x

cur :: (CellIndex -> LensLike f (EditorState a) (EditorState a) b b) -> LensLike f (EditorState a) (EditorState a) b b
cur f l s = (f $ getConst $ currentCell Const s) l s

