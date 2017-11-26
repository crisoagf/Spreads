{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FunctionalDependencies #-}
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
import Data.Bifunctor
import Control.Category hiding ((.), id)

class (Functor f) => ArrayLike f i | f -> i where
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

spreadsheetInferTypes :: (ArrayLike f i, Ord i, Show i, Foldable f) => (NamedReference -> Maybe (CellRawExpr i)) -> f (Either (CellError i) (CellExpr i)) -> f (Either (CellError i) CellType)
spreadsheetInferTypes refMap = spreadsheetFindAndKillLoops >>> spreadsheetInferTypes' refMap

spreadsheetEval :: (ArrayLike f i, Ord i, Show i, Foldable f) => (NamedReference -> Maybe (CellRawExpr i)) -> f (Either (CellError i) (CellExpr i)) -> f (Either (CellError i) (CellWithType i))
spreadsheetEval refMap = spreadsheetFindAndKillLoops >>> spreadsheetEval' refMap

spreadsheetAnalyse :: (Functor f, Ord i) => f (Either (CellError i) (CellExpr i)) -> f (Either (CellError i) (CellExpr i, Set i))
spreadsheetAnalyse arr = fmap (fmap ((,) <$> id <*> cellDeps)) arr

spreadsheetFindLoops :: (ArrayLike f i, Ord i, Foldable f) => f (Either (CellError i) (CellExpr i, Set i)) -> f (Either (CellError i) (CellExpr i, Map i [i]))
spreadsheetFindLoops arr = f $ mapWithIndex (\ i -> fmap (second $ M.fromSet (\x -> [x,i]))) arr
  where f curr = let next = fmap (\ ees' -> do
                              let ees = fmap snd ees'
                              expr <- fmap fst ees'
                              newMap <- M.foldrWithKey (\ descendant path m ->
                                maybe m (
                                  (foldr (\ subdescendant prevm -> M.insertWith const subdescendant (subdescendant : path) prevm) <$> m) <*>) (fmap snd <$> arr `at` descendant)) ees =<< ees
                              if either (const True) (newMap ==) ees then (True,) <$> ees' else pure (False, (expr, newMap))) curr in
                                if foldr (&&) True (fmap (either (const True) fst) next) then curr else fmap (fmap snd) next

spreadsheetKillLoops :: (Functor f, Ord i, ArrayLike f i) => f (Either (CellError i) (CellExpr i, Map i [i])) -> f (Either (CellError i) (CellExpr i))
spreadsheetKillLoops = mapWithIndex (\ i -> (uncurry (\ expr -> maybe (pure expr) (Left . RE . Loop . reverse)) =<<) . fmap (second (M.lookup i)))

spreadsheetFindAndKillLoops :: (Functor f, Ord i, ArrayLike f i, Foldable f) => f (Either (CellError i) (CellExpr i)) -> f (Either (CellError i) (CellExpr i))
spreadsheetFindAndKillLoops = spreadsheetAnalyse >>> spreadsheetFindLoops >>> spreadsheetKillLoops

spreadsheetInferTypes' :: (ArrayLike f i, Ord i, Show i) => (NamedReference -> Maybe (CellRawExpr i)) -> f (Either (CellError i) (CellExpr i)) -> f (Either (CellError i) CellType)
spreadsheetInferTypes' refMap arr = go where go = mapWithIndex (\ ind e -> (either Left ((runSome (InferContext (\ ci -> cellToM ci go) refMap)) . getType ind) e)) arr

spreadsheetEval' :: (ArrayLike f i, Ord i, Show i) => (NamedReference -> Maybe (CellRawExpr i)) -> f (Either (CellError i) (CellExpr i)) -> f (Either (CellError i) (CellWithType i))
spreadsheetEval' refMap arr = go where go = mapWithIndex (\ ind e -> (either Left ((runSome (EvalContext (\ ci -> cellToM ci go) refMap)) . typeAndEvaluate ind) e)) arr

cellToM :: ArrayLike f i => i -> f (Either (CellError i) b) -> ExceptT (CellError i) (Reader a) b
cellToM i arr = maybe (throwE (RE (RefDoesNotExist i))) (ExceptT . pure) (arr `at` i)

