{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Spread.StdLib where
import Spread.TypesAndVals
import Morte.Core
import Data.Text.Lazy
import Control.Monad ((>=>))
import Data.Time

type CellRawExprWError i = Either (CellError i) (CellRawExpr i)

liftHaskFnType :: Text -> Text -> CellRawExpr i -> (CellRawExpr i -> CellExpr i) -> CellRawExpr i
liftHaskFnType fname tname restype f = Embed $ LiftHaskFun tname (Const Star) restype (ShowWrap (unpack fname) f)

liftHaskFnPos :: Show i => Text -> CellRawExpr i -> (i -> CellExpr i) -> CellRawExpr i
liftHaskFnPos fname restype f = Embed $ LiftHaskFun "" (Embed $ TypeValue Pos) restype (ShowWrap (unpack fname)
  (\case
    Embed (PosValue x) -> f x
    _ -> error "Error in Spread lifting algorithm, please file a bug report!"))

liftHaskFnInt :: Show i => Text -> CellRawExpr i -> (Int -> CellExpr i) -> CellRawExpr i
liftHaskFnInt fname restype f = Embed $ LiftHaskFun "" (Embed $ TypeValue Int) restype (ShowWrap (unpack fname)
  (\case
    Embed (IntValue x) -> f x
    _ -> error "Error in Spread lifting algorithm, please file a bug report!"))

liftHaskFnDate :: Show i => Text -> CellRawExpr i -> (Day -> CellExpr i) -> CellRawExpr i
liftHaskFnDate fname restype f = Embed $ LiftHaskFun "" (Embed $ TypeValue Date) restype (ShowWrap (unpack fname)
  (\case
    Embed (DateValue x) -> f x
    _ -> error "Error in Spread lifting algorithm, please file a bug report!"))

liftHaskFnTimeDiff :: Show i => Text -> CellRawExpr i -> (NominalDiffTime -> CellExpr i) -> CellRawExpr i
liftHaskFnTimeDiff fname restype f = Embed $ LiftHaskFun "" (Embed $ TypeValue TimeDiff) restype (ShowWrap (unpack fname)
  (\case
    Embed (TimeDiffValue x) -> f x
    _ -> error "Error in Spread lifting algorithm, please file a bug report!"))

liftHaskFnTime :: Show i => Text -> CellRawExpr i -> (UTCTime -> CellExpr i) -> CellRawExpr i
liftHaskFnTime fname restype f = Embed $ LiftHaskFun "" (Embed $ TypeValue Time) restype (ShowWrap (unpack fname)
  (\case
    Embed (TimeValue x) -> f x
    _ -> error "Error in Spread lifting algorithm, please file a bug report!"))
