{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
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
import Spread.StdLib.Ind

data Visions = Input | Value | Presentation

data CellType =
  IntType                    |
  FloatType                  |
  StringType                 |
  DateType                   |
  TimeType                   |
  ListType CellType          |
  FuncType CellType CellType deriving (Show, Eq)

data CellRawValue =
  IntValue Int                          |
  FloatValue Double                     |
  StringValue Text                      |
  DateValue Day                         |
  TimeValue UTCTime                     |
  ListValue [CellRawValue]              |
  FuncValue Variable CellType CellValue deriving Show

newtype ShowWrap w = ShowWrap w

instance Show (ShowWrap w) where
  show (ShowWrap _) = "(hidden)"

data CellValue =
  RawValue CellRawValue                                                                            |
  LiftHaskFun CellType CellType (ShowWrap ((CellType -> CellValue -> Either TypeError CellRawValue) -> CellValue -> CellValue)) |
  VariableValue Text                                                                               |
  NamedRefValue NamedReference                                                                     |
  Range [CellIndex]                                                                                |
  App CellValue CellValue deriving Show

data TypeError =
  FreeVar                            |
  FuncBodyDoesNotTypecheck TypeError |
  AppWithoutFunc                     |
  AppMismatch TypeError              |
  NoSuchName                         |
  VarTypeMismatch                    |
  ValTypeMismatch                    |
  NonHomogeneousList                 |
  InferEmptyList                     |
  GrossMismatch deriving Show

type Variable = Text
type NamedReference = Text
type CellIndex = (Natural, Natural)

type SpreadsheetM = EitherT TypeError (Reader (Array CellIndex CellValue))

typecheck :: Map Variable CellType -> Map NamedReference CellValue -> (CellIndex -> Either TypeError CellRawValue) -> CellType -> CellValue -> Maybe TypeError
typecheck _      _     _     IntType                   (RawValue (IntValue    x            )) = Nothing
typecheck _      _     _     FloatType                 (RawValue (FloatValue  x            )) = Nothing
typecheck _      _     _     StringType                (RawValue (StringValue x            )) = Nothing
typecheck _      _     _     DateType                  (RawValue (DateValue   x            )) = Nothing
typecheck _      _     _     TimeType                  (RawValue (TimeValue   x            )) = Nothing
typecheck varMap names vals (ListType valType)         (RawValue (ListValue   x            )) = asum $ map (typecheck varMap names vals valType . RawValue) x
typecheck varMap names vals (FuncType fromType toType) (RawValue (FuncValue var varType val)) = typecheck (M.insert var varType varMap) names vals toType val <|> if fromType == varType then Nothing else Just VarTypeMismatch
typecheck varMap names vals  retType                   (NamedRefValue refName) = maybe (Just NoSuchName) (typecheck varMap names vals retType) (M.lookup refName names)
typecheck varMap names vals (ListType retType)         (Range indList) = asum $ map (either Just (typecheck varMap names vals retType . RawValue) . vals) indList
typecheck varMap names vals  retType                   (App (RawValue (FuncValue var varType val)) x) = typecheck varMap names vals varType x <|> typecheck (M.insert var varType varMap) names vals retType val
typecheck varMap names vals  retType                   (App (LiftHaskFun varType valType f) x) = typecheck varMap names vals varType x <|> if retType == valType then Nothing else Just ValTypeMismatch
typecheck _ _ _ _ (App _ _) = Just AppWithoutFunc
typecheck varMap _ _ varType (VariableValue varName) = maybe (Just FreeVar) (\ x -> if x == varType then Nothing else Just VarTypeMismatch) (M.lookup varName varMap)
typecheck _ _ _ _ _ = Just GrossMismatch

evaluateWithContext :: Map NamedReference CellValue -> (CellIndex -> Either TypeError CellRawValue) -> CellType -> CellValue -> Either TypeError CellRawValue 
evaluateWithContext _     _     IntType                   (RawValue (IntValue    x            )) = Right (IntValue    x)
evaluateWithContext _     _     FloatType                 (RawValue (FloatValue  x            )) = Right (FloatValue  x)
evaluateWithContext _     _     StringType                (RawValue (StringValue x            )) = Right (StringValue x)
evaluateWithContext _     _     DateType                  (RawValue (DateValue   x            )) = Right (DateValue   x)
evaluateWithContext _     _     TimeType                  (RawValue (TimeValue   x            )) = Right (TimeValue   x)
evaluateWithContext names vals (ListType valType)         (RawValue (ListValue   x            )) = ListValue <$> mapM (evaluateWithContext names vals valType . RawValue) x
evaluateWithContext names vals (FuncType fromType toType) (RawValue (FuncValue var varType val)) =
  maybe (Right (FuncValue var varType val)) (Left . FuncBodyDoesNotTypecheck) (typecheck (M.singleton var varType) names vals toType val <|> if fromType == varType then Nothing else Just VarTypeMismatch)
evaluateWithContext names vals  retType                   (NamedRefValue refName) = maybe (Left NoSuchName) (evaluateWithContext names vals retType) (M.lookup refName names)
evaluateWithContext names vals (ListType retType)         (Range indList) = ListValue <$> mapM (evaluateWithContext names vals retType . RawValue <=< vals) indList
evaluateWithContext names vals  retType                   (App (RawValue (FuncValue var varType val)) x) =
  maybe (evaluateWithContext (M.insert var x names) vals retType (replaceExpr var (NamedRefValue var) val)) (Left . AppMismatch) (typecheck M.empty names vals varType x <|> typecheck (M.singleton var varType) names vals retType val)
evaluateWithContext names vals  retType                   (App (LiftHaskFun varType valType (ShowWrap f)) x) =
  maybe (evaluateWithContext names vals retType $ f (evaluateWithContext names vals) x) Left $ typecheck M.empty names vals varType x <|> if retType == valType then Nothing else Just ValTypeMismatch
evaluateWithContext names vals  retType                   (App (NamedRefValue refName) x) = maybe (Left NoSuchName) (\ new -> evaluateWithContext names vals retType (App new x)) (M.lookup refName names)
evaluateWithContext _ _ _ (App _ _) = Left AppWithoutFunc
evaluateWithContext _ _ _ (VariableValue _) = Left FreeVar
evaluateWithContext _ _ _ _ = Left GrossMismatch

inferType :: Map Variable CellType -> Map NamedReference CellValue -> (CellIndex -> Either TypeError CellType) -> CellValue -> Either TypeError CellType
inferType _      _      cellMap (RawValue (IntValue    _            )) = pure IntType
inferType _      _      cellMap (RawValue (FloatValue  _            )) = pure FloatType
inferType _      _      cellMap (RawValue (StringValue _            )) = pure StringType
inferType _      _      cellMap (RawValue (DateValue   _            )) = pure DateType
inferType _      _      cellMap (RawValue (TimeValue   _            )) = pure TimeType
inferType varMap refMap cellMap (RawValue (ListValue   x            )) = ListType <$> ((\case
  [] -> Left InferEmptyList
  (t:ts) -> foldr (\ nextType ePrevType -> (\ prevType -> if prevType == nextType then pure prevType else Left NonHomogeneousList) =<< ePrevType) (pure t) ts) =<< mapM (inferType varMap refMap cellMap . RawValue) x)
inferType varMap refMap cellMap (RawValue (FuncValue var varType val)) = FuncType varType <$> inferType (M.insert var varType varMap) refMap cellMap val
inferType varMap refMap cellMap (NamedRefValue refName) = maybe (Left NoSuchName) (inferType varMap refMap cellMap) (M.lookup refName refMap)
inferType varMap refMap cellMap (Range indList) = ListType <$> ((\case
  [] -> Left InferEmptyList
  (t:ts) -> foldr (\ nextType ePrevType -> do
    prevType <- ePrevType
    when (prevType /= nextType) (Left NonHomogeneousList)
    ePrevType) (pure t) ts) =<< mapM cellMap indList)
inferType varMap refMap cellMap (App (RawValue (FuncValue var varType val)) x) = do
  replacingType <- inferType varMap refMap cellMap x
  when (replacingType /= varType) (Left VarTypeMismatch)
  inferType (M.insert var varType varMap) refMap cellMap val
inferType varMap refMap cellMap (App (LiftHaskFun varType valType (ShowWrap f)) x) = do
  replacingType <- inferType varMap refMap cellMap x
  when (replacingType /= varType) (Left VarTypeMismatch)
  pure valType
inferType varMap refMap cellMap (App (NamedRefValue refName) x) = maybe (Left NoSuchName) (\ new -> inferType varMap refMap cellMap (App new x)) (M.lookup refName refMap)
inferType varMap _ _ (VariableValue varName) = maybe (Left FreeVar) Right (M.lookup varName varMap)
inferType _ _ _ (App _ _) = Left AppWithoutFunc
inferType _ _ _ _ = Left GrossMismatch

replaceExpr :: Variable -> CellValue -> CellValue -> CellValue
replaceExpr name newVal orig@(VariableValue name') = if name == name' then newVal else orig
replaceExpr name newVal orig@(RawValue (FuncValue var varType val)) = if name == var then orig else RawValue (FuncValue var varType (replaceExpr name newVal val))
replaceExpr name newVal (App x y) = App (replaceExpr name newVal x) (replaceExpr name newVal y)
replaceExpr _ _ x = x

spreadsheetInferTypes :: Map NamedReference CellValue -> Array CellIndex CellValue -> Array CellIndex (Either TypeError (CellType, CellValue))
spreadsheetInferTypes refMap arr = go where go = fmap (\ v -> (,v) <$> inferType M.empty refMap (fmap fst . (go !)) v) arr

spreadsheetEval :: Map NamedReference CellValue -> Array CellIndex (Either TypeError (CellType, CellValue)) -> Array CellIndex (Either TypeError CellRawValue)
spreadsheetEval refMap arr = go where go = fmap ((\ (t, v) -> evaluateWithContext refMap (go !) t v) =<<) arr

exampleSheet :: Array (Natural, Natural) CellValue
exampleSheet = listArray ((0,0),(2,2))
  [ RawValue $ IntValue 1, RawValue $ IntValue 2, RawValue $ IntValue 3
  , RawValue $ IntValue 4, RawValue $ IntValue 5, RawValue $ IntValue 6
  , RawValue $ IntValue 7, RawValue $ IntValue 8, App (NamedRefValue "sum") (Range (range ((0,0),(1,1))))]

stdLib :: Map NamedReference CellValue
stdLib = M.fromList basicIntFns

