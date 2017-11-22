{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Spread.StdLib where
import Spread.TypesAndVals
import Morte.Core
import Data.Text.Lazy
import Control.Monad ((>=>))
import Data.Time

type CellRawExprWError i = Either (CellError i) (CellRawExpr i)

liftHaskFnType :: Text -> Text -> CellRawExpr i -> (CellRawExpr i -> CellRawExprWError i) -> CellRawExpr i
liftHaskFnType fname tname restype f = Embed $ LiftHaskFun tname (Const Star) restype (ShowWrap (unpack fname) (\ evalFn ->
    (evalFn . Left) >=>
    (\case
      CellWithType (Const Star) expr -> f expr
      CellWithType t _ -> error (unpack fname ++ ": The Spread type system has failed us! We wanted a Type, but it gave us a " ++ show t))))

liftHaskFnPos :: Show i => Text -> CellRawExpr i -> ((Either (CellExpr i) i -> Either (CellError i) (CellWithType i)) -> i -> CellRawExprWError i) -> CellRawExpr i
liftHaskFnPos fname restype f = Embed $ LiftHaskFun "" (Embed $ TypeValue Pos) restype (ShowWrap (unpack fname) (\ evalFn ->
    (evalFn . Left) >=>
    (\case
      CellWithType (Embed Pos) (Embed (PosValue i)) -> f evalFn i
      CellWithType t _ -> error (unpack fname ++ ": The Spread type system has failed us! We wanted a Pos, but it gave us a " ++ show t))))

liftHaskFnInt :: Show i => Text -> CellRawExpr i -> ((Either (CellExpr i) i -> Either (CellError i) (CellWithType i)) -> Int -> CellRawExprWError i) -> CellRawExpr i
liftHaskFnInt fname restype f = Embed $ LiftHaskFun "" (Embed $ TypeValue Int) restype (ShowWrap (unpack fname) (\ evalFn ->
    (evalFn . Left) >=>
    (\case
      CellWithType (Embed Int) (Embed (IntValue i)) -> f evalFn i
      CellWithType t _ -> error (unpack fname ++ ": The Spread type system has failed us! We wanted an Int, but it gave us a " ++ show t))))

liftHaskFnDate :: Show i => Text -> CellRawExpr i -> ((Either (CellExpr i) i -> Either (CellError i) (CellWithType i)) -> Day -> CellRawExprWError i) -> CellRawExpr i
liftHaskFnDate fname restype f = Embed $ LiftHaskFun "" (Embed $ TypeValue Date) restype (ShowWrap (unpack fname) (\ evalFn ->
    (evalFn . Left) >=>
    (\case
      CellWithType (Embed Date) (Embed (DateValue i)) -> f evalFn i
      CellWithType t _ -> error (unpack fname ++ ": The Spread type system has failed us! We wanted a Date, but it gave us a " ++ show t))))

liftHaskFnTimeDiff :: Show i => Text -> CellRawExpr i -> ((Either (CellExpr i) i -> Either (CellError i) (CellWithType i)) -> NominalDiffTime -> CellRawExprWError i) -> CellRawExpr i
liftHaskFnTimeDiff fname restype f = Embed $ LiftHaskFun "" (Embed $ TypeValue TimeDiff) restype (ShowWrap (unpack fname) (\ evalFn ->
    (evalFn . Left) >=>
    (\case
      CellWithType (Embed TimeDiff) (Embed (TimeDiffValue i)) -> f evalFn i
      CellWithType t _ -> error (unpack fname ++ ": The Spread type system has failed us! We wanted a TimeDiff, but it gave us a " ++ show t))))

liftHaskFnTime :: Show i => Text -> CellRawExpr i -> ((Either (CellExpr i) i -> Either (CellError i) (CellWithType i)) -> UTCTime -> CellRawExprWError i) -> CellRawExpr i
liftHaskFnTime fname restype f = Embed $ LiftHaskFun "" (Embed $ TypeValue Time) restype (ShowWrap (unpack fname) (\ evalFn ->
    (evalFn . Left) >=>
    (\case
      CellWithType (Embed Time) (Embed (TimeValue i)) -> f evalFn i
      CellWithType t _ -> error (unpack fname ++ ": The Spread type system has failed us! We wanted a TimeDiff, but it gave us a " ++ show t))))
