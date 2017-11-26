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
    \t -> fmap RawValue $ liftHaskFnPos ("Range " <> pretty t) (Pi "" (Embed $ TypeValue Pos) (listTypeOf t)) $
      \ i -> fmap RawValue $ liftHaskFnPos ("Range " <> pretty t <> " " <> (pack $ show i)) (listTypeOf t) $
        \ j -> listValueOf (fmap RawValue t) ((Embed . RefValue) <$> (range (i,j))))]

