{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module Spreads.Evaluate where
import Data.Text.Lazy (Text)
import Spreads.Basic
import Spreads.Vision
import Spreads.Editor.State
import Spread.StdLib.Int
import Spread.StdLib.Range
import Spread.StdLib.DateTime
import Control.Lens
import LensCheats

import Spread hiding (at)

prettify :: Vision a -> VisionType a -> Text
prettify VisionInput = pretty . ErrorWrap
prettify VisionType  = pretty . ErrorWrap
prettify VisionValue = pretty . ErrorWrap . fmap cellValue
prettify VisionPres  = const "TODO: Define presentation layer!"

expressionise :: Vision b -> Spreadsheet Text -> Spreadsheet (VisionType b)
expressionise VisionInput = spreadsheetParse
expressionise VisionType  = spreadsheetInferTypes stdLib . spreadsheetParse
expressionise VisionValue = spreadsheetEval stdLib . spreadsheetParse
expressionise VisionPres  = fmap (const ())

cellExprise :: Vision b -> VisionType b -> Either CellError CellExpr
cellExprise VisionInput = id
cellExprise VisionType  = fmap (fmap $ RawValue . TypeValue)
cellExprise VisionValue = fmap (fmap RawValue . cellValue)
cellExprise VisionPres  = const (Left $ refError $ emptyError)

stdLib :: NamedReference -> Maybe CellRawExpr
stdLib = flip lookup (basicIntFns ++ basicRangeFns ++ basicDateFns)

visualiseCellWith :: VisionToken -> CellIndex -> Getter (EditorState a) (Maybe Text)
visualiseCellWith v x = applying (notRelevant visualise v) currentSheet . (at x)

expressioniseCellWith :: Vision b -> CellIndex -> Getter (EditorState a) (Maybe (VisionType b))
expressioniseCellWith v x = applying (expressionise v) currentSheet . (at x)

visualise :: Vision a -> Spreadsheet Text -> Spreadsheet Text
visualise x = fmap (prettify x) . expressionise x

