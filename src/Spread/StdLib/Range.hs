{-# LANGUAGE OverloadedStrings #-}
module Spread.StdLib.Range where
import Spread.StdLib
import Spread.TypesAndVals
import Morte.Core
import Data.Text.Lazy
import Data.Maybe
import Data.Ix
import Data.Text.Buildable
import Data.Monoid

basicRangeFns :: [(NamedReference, CellRawExpr)]
basicRangeFns = [
  ("Range", liftHaskFnType "Range" "a" (Pi "" (Embed Pos) (Pi "" (Embed Pos) (App (Embed List) (Var $ V "a" 0)))) $
    \t -> liftHaskFnPos ("Range " <> pretty t) (Pi "" (Embed Pos) (App (Embed List) t)) $
      \ _ i -> liftHaskFnPos ("Range " <> pretty t <> " " <> (pack $ show i)) (App (Embed List) t) $
        \ evalFn j -> Embed (ListValue t (either (error "Range i j: The Spread type system has failed us, it gave us something that does not type check!") (mapMaybe (\ (CellWithType t' v) -> if t' == t then Just v else Nothing)) $ mapM (evalFn . Right) (range (i,j))))) --,
  --("RangeInt", App (Embed (NamedRefValue "Range")) (Embed (RawValue $ TypeValue (Embed Int))))
  ]

