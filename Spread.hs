{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Spread where
import Control.Monad.Trans.Either
import Control.Monad.Reader
import Data.Foldable
import Control.Applicative
import Data.Array
import Data.Map (Map)
import qualified Data.Map as M
import Numeric.Natural
import Data.Time
import Data.Time.Calendar
import Data.Text (Text)
import Spread.StdLib.Int
import Spread.TypesAndVals
import Data.Bifunctor

data Visions = Input | Value | Presentation

data TypeError =
  FreeVar                            |
  FuncBodyDoesNotTypecheck TypeError |
  AppWithoutFunc                     |
  AppMismatch TypeError (CellValue TypeError) |
  NoSuchName                         |
  VarTypeMismatch                    |
  ValTypeMismatch                    |
  NonHomogeneousList                 |
  InferEmptyList                     |
  GrossMismatch (CellValue TypeError) deriving Show

type SpreadsheetM = EitherT TypeError (Reader (Array CellIndex (CellValue TypeError)))

--typecheck :: Map Variable CellType -> Map NamedReference (CellValue TypeError) -> (CellIndex -> Either TypeError CellType) -> CellType -> (CellValue TypeError) -> Either TypeError CellType
--typecheck _      _     _     IntType                   (RawValue (IntValue    x            )) = Nothing
--typecheck _      _     _     FloatType                 (RawValue (FloatValue  x            )) = Nothing
--typecheck _      _     _     StringType                (RawValue (StringValue x            )) = Nothing
--typecheck _      _     _     DateType                  (RawValue (DateValue   x            )) = Nothing
--typecheck _      _     _     TimeType                  (RawValue (TimeValue   x            )) = Nothing
--typecheck varMap names vals (ListType valType)         (RawValue (ListValue   x            )) = asum $ map (typecheck varMap names vals valType . RawValue) x
--typecheck varMap names vals (FuncType fromType toType) (RawValue (FuncValue var varType val)) = typecheck (M.insert var varType varMap) names vals toType val <|> if fromType == varType then Nothing else Just VarTypeMismatch
--typecheck varMap names vals  retType                   (NamedRefValue refName) = maybe (Just NoSuchName) (typecheck varMap names vals retType) (M.lookup refName names)
--typecheck varMap names vals (ListType retType)         (Range indList) = asum $ map (either Just (typecheck varMap names vals retType . RawValue) . vals) indList
--typecheck varMap names vals  retType                   (App (RawValue (FuncValue var varType val)) x) = typecheck varMap names vals varType x <|> typecheck (M.insert var varType varMap) names vals retType val
--typecheck varMap names vals  retType                   (App (LiftHaskFun varType valType f) x) = typecheck varMap names vals varType x <|> if retType == valType then Nothing else Just ValTypeMismatch
--typecheck _ _ _ _ (App _ _) = Just AppWithoutFunc
--typecheck varMap _ _ varType (VariableValue varName) = maybe (Just FreeVar) (\ x -> if x == varType then Nothing else Just VarTypeMismatch) (M.lookup varName varMap)
--typecheck _ _ _ _ expr = Just (GrossMismatch expr)

evaluate :: Map NamedReference (CellValue TypeError) -> (CellIndex -> Either TypeError (CellType, CellRawValue TypeError)) -> CellValue TypeError -> Either TypeError (CellType, CellRawValue TypeError) 
evaluate _     _    (RawValue (IntValue    x            )) = Right (IntType   , IntValue    x)
evaluate _     _    (RawValue (FloatValue  x            )) = Right (FloatType , FloatValue  x)
evaluate _     _    (RawValue (StringValue x            )) = Right (StringType, StringValue x)
evaluate _     _    (RawValue (DateValue   x            )) = Right (DateType  , DateValue   x)
evaluate _     _    (RawValue (TimeValue   x            )) = Right (TimeType  , TimeValue   x)
evaluate names vals (RawValue (ListValue   x            )) = bimap ListType ListValue <$> ((\case
  [] -> Left InferEmptyList
  (a:as) -> foldrM (\ (nextType, nextVal) (prevType, prevValList) -> if nextType /= prevType then Left NonHomogeneousList else pure (prevType, nextVal : prevValList)) (second (const []) a) (a:as)) =<< mapM (evaluate names vals . RawValue) x)
evaluate names vals (RawValue (FuncValue var varType val)) =
  do
    toType <- first FuncBodyDoesNotTypecheck $ inferType (M.singleton var varType) names (fmap fst . vals) val
    pure (FuncType varType toType, FuncValue var varType val)
evaluate names vals (RawValue (LiftHaskFun fromType toType x)) = pure (FuncType fromType toType, LiftHaskFun fromType toType x)
evaluate names vals (NamedRefValue refName) = maybe (Left NoSuchName) (evaluate names vals) (M.lookup refName names)
evaluate names vals (Range indList) = bimap ListType ListValue <$> ((\case
  [] -> Left InferEmptyList
  (a:as) -> foldrM (\ (nextType, nextVal) (prevType, prevValList) -> if nextType /= prevType then Left NonHomogeneousList else pure (prevType, nextVal : prevValList)) (second (const []) a) (a:as)) =<< mapM (evaluate names vals . RawValue . snd <=< vals) indList)
evaluate names vals expr@(App (RawValue (FuncValue var varType val)) x) =
  do
    replacingType <- inferType M.empty names (fmap fst . vals) x
    bodyType <- first FuncBodyDoesNotTypecheck $ inferType (M.singleton var varType) names (fmap fst . vals) val
    when (replacingType /= varType) (Left (AppMismatch VarTypeMismatch expr))
    evaluate (M.insert var x names) vals (replaceExpr var (NamedRefValue var) val)
evaluate names vals expr@(App (RawValue (LiftHaskFun varType valType (ShowWrap f))) x) = do
  replacingType <- inferType M.empty names (fmap fst . vals) x
  when (replacingType /= varType) (Left (AppMismatch VarTypeMismatch expr))
  evaluate names vals (f (evaluate names vals) x)
evaluate names vals (App (NamedRefValue refName) x) = maybe (Left NoSuchName) (\ new -> evaluate names vals (App new x)) (M.lookup refName names)
evaluate names vals (App (App x y) z) = (\ (newType, new) -> evaluate names vals (App (RawValue new) z)) =<< evaluate names vals (App x y)
evaluate _ _ (App _ _) = Left AppWithoutFunc
evaluate _ _ (VariableValue _) = Left FreeVar
--evaluate _ _ expr = Left (GrossMismatch expr)

inferType :: Map Variable CellType -> Map NamedReference (CellValue TypeError) -> (CellIndex -> Either TypeError CellType) -> (CellValue TypeError) -> Either TypeError CellType
inferType _      _      cellMap (RawValue (IntValue    _            )) = pure IntType
inferType _      _      cellMap (RawValue (FloatValue  _            )) = pure FloatType
inferType _      _      cellMap (RawValue (StringValue _            )) = pure StringType
inferType _      _      cellMap (RawValue (DateValue   _            )) = pure DateType
inferType _      _      cellMap (RawValue (TimeValue   _            )) = pure TimeType
inferType varMap refMap cellMap (RawValue (ListValue   x            )) = ListType <$> ((\case
  [] -> Left InferEmptyList
  (t:ts) -> foldr (\ nextType ePrevType -> (\ prevType -> if prevType == nextType then pure prevType else Left NonHomogeneousList) =<< ePrevType) (pure t) ts) =<< mapM (inferType varMap refMap cellMap . RawValue) x)
inferType _ _ _ (RawValue (LiftHaskFun fromType toType _)) = pure (FuncType fromType toType)
inferType varMap refMap cellMap (RawValue (FuncValue var varType val)) = FuncType varType <$> inferType (M.insert var varType varMap) refMap cellMap val
inferType varMap refMap cellMap (NamedRefValue refName) = maybe (Left NoSuchName) (inferType varMap refMap cellMap) (M.lookup refName refMap)
inferType varMap refMap cellMap (Range indList) = ListType <$> ((\case
  [] -> Left InferEmptyList
  (t:ts) -> foldr (\ nextType ePrevType -> do
    prevType <- ePrevType
    when (prevType /= nextType) (Left NonHomogeneousList)
    ePrevType) (pure t) ts) =<< mapM cellMap indList)
inferType varMap refMap cellMap expr@(App x y) = do
  (\case
    FuncType t1 t2 -> (\ ty -> if ty == t1 then pure t2 else Left (AppMismatch VarTypeMismatch expr)) =<< inferType varMap refMap cellMap y
    _ -> Left AppWithoutFunc) =<< inferType varMap refMap cellMap x
inferType varMap _ _ (VariableValue varName) = maybe (Left FreeVar) Right (M.lookup varName varMap)
--inferType _ _ _ expr = Left (GrossMismatch expr)

replaceExpr :: Variable -> (CellValue TypeError) -> (CellValue TypeError) -> (CellValue TypeError)
replaceExpr name newVal orig@(VariableValue name') = if name == name' then newVal else orig
replaceExpr name newVal orig@(RawValue (FuncValue var varType val)) = if name == var then orig else RawValue (FuncValue var varType (replaceExpr name newVal val))
replaceExpr name newVal (App x y) = App (replaceExpr name newVal x) (replaceExpr name newVal y)
replaceExpr _ _ x = x

spreadsheetInferTypes :: Map NamedReference (CellValue TypeError) -> Array CellIndex (CellValue TypeError) -> Array CellIndex (Either TypeError (CellType, (CellValue TypeError)))
spreadsheetInferTypes refMap arr = go where go = fmap (\ v -> (,v) <$> inferType M.empty refMap (fmap fst . (go !)) v) arr

spreadsheetEval :: Map NamedReference (CellValue TypeError) -> Array CellIndex (CellValue TypeError) -> Array CellIndex (Either TypeError (CellType, CellRawValue TypeError))
spreadsheetEval refMap arr = go where go = fmap (evaluate refMap (go !)) arr

exampleSheet :: Array (Natural, Natural) (CellValue TypeError)
exampleSheet = listArray ((0,0),(2,2))
  [ RawValue $ IntValue 1, RawValue $ IntValue 2, RawValue $ IntValue 3
  , RawValue $ IntValue 4, RawValue $ IntValue 5, RawValue $ IntValue 6
  , RawValue $ IntValue 7, App (App (NamedRefValue "plus") (RawValue $ IntValue 8)) (RawValue $ IntValue 5), App (NamedRefValue "sum") (Range (range ((0,0),(1,1))))]

stdLib :: Map NamedReference (CellValue TypeError)
stdLib = M.fromList basicIntFns

