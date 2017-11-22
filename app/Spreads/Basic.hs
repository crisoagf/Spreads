{-# LANGUAGE LambdaCase #-}
module Spreads.Basic (
  CellType,
  CellIndex (..),
  NamedReference,
  CellExpr,
  CellRawExpr,
  CellWithType,
  CellError,
  cellValue,
  ErrorWrap (..),
  Spreadsheet,
  FullSpreadsheet,
  CellValue (..),
  emptyError,
  CellRawValue (..),
  refError,
  alterCell,
  shiftIndices,
  pretty,
  save,
  load,
  fromList) where
import qualified Spread.TypesAndVals as TsAndVals
import Spread.TypesAndVals (CellIndex(..), ErrorWrap(..), CellType, NamedReference, CellValue ( RawValue ), CellRawValue (TypeValue))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text.Lazy (Text)
import Morte.Core
import Data.Binary

type FullSpreadsheet = ((CellIndex, CellIndex), Spreadsheet Text)

type CellExpr     = TsAndVals.CellExpr     CellIndex
type CellRawExpr  = TsAndVals.CellRawExpr  CellIndex
type CellWithType = TsAndVals.CellWithType CellIndex
type CellError    = TsAndVals.CellError    CellIndex

cellValue :: CellWithType -> CellRawExpr
cellValue = TsAndVals.cellValue

type Spreadsheet a = Map CellIndex a

refError :: TsAndVals.RefError i -> TsAndVals.CellError i
refError = TsAndVals.RE

emptyError :: TsAndVals.RefError i
emptyError = TsAndVals.Empty

alterCell :: (Maybe a -> Maybe a) -> CellIndex -> Spreadsheet a -> Spreadsheet a
alterCell = M.alter

shiftIndices :: CellIndex -> CellIndex -> CellExpr -> CellExpr
shiftIndices (CellIndex (t1,t2)) (CellIndex (o1,o2)) = fmap (\case
  TsAndVals.RawValue (TsAndVals.PosValue (CellIndex (a,b))) -> TsAndVals.RawValue (TsAndVals.PosValue (CellIndex (t1 + a - o1, t2 + b - o2)))
  TsAndVals.RefValue (CellIndex (a,b)) -> TsAndVals.RefValue (CellIndex (t1 + a - o1, t2 + b - o2))
  x -> x)

fromList :: [(CellIndex, a)] -> Spreadsheet a
fromList = M.fromList

save :: Maybe FilePath -> FullSpreadsheet -> IO Bool
save Nothing   = const (pure False)
save (Just fp) = (fmap (const True)) . encodeFile fp

load :: Maybe FilePath -> IO (Maybe FullSpreadsheet)
load Nothing   = pure Nothing
load (Just fp) = fmap (either (const Nothing) Just) $ decodeFileOrFail fp

