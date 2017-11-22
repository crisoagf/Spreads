{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE BangPatterns #-}
module Spread where
import Spread.Parser
import Spread.Typecheck
import Spread.TypesAndVals
import Data.Text.Lazy (Text)
import Data.Array
import Data.Set (Set)
import Data.Map (Map)
import qualified Data.Map as M
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except

class (Functor f) => ArrayLike f i where
  mapWithIndex :: (i -> a -> b) -> f a -> f b
  at :: f a -> i -> Maybe a

instance Ix i => ArrayLike (Array i) i where
  mapWithIndex f arr = listArray (bounds arr) $ fmap (uncurry f) $ assocs arr
  at arr i = if inRange (bounds arr) i then Just (arr ! i) else Nothing
instance Ord i => ArrayLike (Map i) i where
  mapWithIndex = M.mapWithKey
  at = flip M.lookup

spreadsheetParse :: Functor f => f Text -> f (Either (CellError CellIndex) (CellExpr CellIndex))
spreadsheetParse = fmap (exprFromText "input")

spreadsheetInferTypes :: (ArrayLike f i, Ord i, Show i) => (NamedReference -> Maybe (CellRawExpr i)) -> f (Either (CellError i) (CellExpr i)) -> f (Either (CellError i) CellType)
spreadsheetInferTypes refMap arr = spreadsheetInferTypes' refMap arr

spreadsheetEval :: (ArrayLike f i, Ord i, Show i) => (NamedReference -> Maybe (CellRawExpr i)) -> f (Either (CellError i) (CellExpr i)) -> f (Either (CellError i) (CellWithType i))
spreadsheetEval refMap arr = spreadsheetEval' refMap arr

spreadsheetAnalyse :: (ArrayLike f i, Ord i, Show i) => f (Either (CellError i) (CellExpr i)) -> i -> Either (CellError i) (Set i)
spreadsheetAnalyse arr i = fmap cellDeps $ maybe (Left $ RE $ RefDoesNotExist i) id $ arr `at` i

spreadsheetInferTypes' :: (ArrayLike f i, Ord i, Show i) => (NamedReference -> Maybe (CellRawExpr i)) -> f (Either (CellError i) (CellExpr i)) -> f (Either (CellError i) CellType)
spreadsheetInferTypes' refMap arr = go where go = mapWithIndex (\ !ind !e -> (either Left ((runSome (InferContext (\ ind' -> either (getType ind') (\ ci -> cellToM ci go)) refMap (spreadsheetAnalyse arr))) . getType ind) e)) arr

spreadsheetEval' :: (ArrayLike f i, Ord i, Show i) => (NamedReference -> Maybe (CellRawExpr i)) -> f (Either (CellError i) (CellExpr i)) -> f (Either (CellError i) (CellWithType i))
spreadsheetEval' refMap arr = go where go = mapWithIndex (\ !ind !e -> (either Left ((runSome (EvalContext (\ ind' -> either (typeAndEvaluate ind') (\ ci -> cellToM ci go)) refMap (spreadsheetAnalyse arr))) . typeAndEvaluate ind) e)) arr

cellToM :: ArrayLike f i => i -> f (Either (CellError i) b) -> ExceptT (CellError i) (Reader a) b
cellToM i arr = maybe (throwE (RE (RefDoesNotExist i))) (ExceptT . pure) (arr `at` i)

