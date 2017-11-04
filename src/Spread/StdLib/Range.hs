{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Spread.StdLib.Range where
import Spread.TypesAndVals
import Data.Maybe
import Data.Ix

basicRangeFns :: [(NamedReference, CellValue errortype)]
basicRangeFns = [
  ("RangeInt", RawValue (LiftHaskFun PosType (FuncType PosType (ListType IntType)) (ShowWrap "Range" (\ evalFn -> either
    (error "RangeInt: The Spread type system has failed us, it gave us something that does not type check!")
    (\case
        (PosType, PosValue i) -> RawValue $ LiftHaskFun PosType (ListType IntType) (ShowWrap ("RangeInt " ++ show i) (\ evalFn -> either
          (error "RangeInt: The Spread type system has failed us, it gave us something that does not type check!")
          (\ case
            (PosType, PosValue j) -> RawValue $ ListValue $ mapMaybe (either (const Nothing) (\case
              (IntType, v) -> Just v
              _ -> Nothing)) $ fmap (evalFn . Right) (range (i,j))
            x -> error ("RangeInt: The Spread type system has failed us! We wanted a Pos, but it gave us " ++ show x)) . evalFn . Left))
        x -> error ("RangeInt: The Spread type system has failed us! We wanted a Pos, but it gave us " ++ show x)) . evalFn . Left))))
  ]
