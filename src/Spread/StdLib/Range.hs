{-# LANGUAGE OverloadedStrings #-}
module Spread.StdLib.Range where
import Spread.StdLib
import Spread.TypesAndVals
import Morte.Core
import Data.Text.Lazy
import Data.Ix
import Data.Monoid
import Data.Text.Buildable

basicRangeFns :: (Eq i, Buildable i, Show i, Ix i) => [(NamedReference, CellRawExpr i)]
basicRangeFns = [
  ("Range", liftHaskFnType "Range" "a" (Pi "" (Embed (TypeValue Pos)) (Pi "" (Embed (TypeValue Pos)) (listTypeOf (Var $ V "a" 0)))) $
    \t -> Right $ liftHaskFnPos ("Range " <> pretty t) (Pi "" (Embed $ TypeValue Pos) (listTypeOf t)) $
      \ _ i -> Right $ liftHaskFnPos ("Range " <> pretty t <> " " <> (pack $ show i)) (listTypeOf t) $
        \ evalFn j -> listValueOf t <$> mapM (uncurry (\ refInd -> either
          (Left . RE . ErrorInRef refInd) 
          (\ (CellWithType cellType' cellVal) ->
            let cellTyp = fmap TypeValue cellType' in
              if cellTyp == t then pure cellVal else Left (RE $ WrongTypeInRef t cellTyp))) . ((,) <$> id <*> evalFn . Left . Embed . RefValue)) (range (i,j)))] --,
  --("RangeInt", App (Embed (NamedRefValue "Range")) (Embed (RawValue $ TypeValue (Embed Int))))

