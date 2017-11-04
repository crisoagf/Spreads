{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Spread where
import Data.Array
import Data.Map (Map)
import qualified Data.Map as M
import Numeric.Natural
import Spread.StdLib.Int
import Spread.StdLib.Range
import Spread.TypesAndVals
import Spread.Typecheck
import Spread.Parser
import Control.Monad.Trans.Except
import Control.Monad.Trans.State

data Visions = Input | Value | Presentation

spreadsheetInferTypes :: Map NamedReference (CellValue TypeError) -> Array CellIndex (CellValue TypeError) -> Array CellIndex (Either TypeError (CellType, (CellValue TypeError)))
spreadsheetInferTypes refMap arr = go where go = fmap (\ v -> (,v) <$> (evalState (runExceptT (inferType M.empty refMap (fmap fst . (go !)) v)) M.empty)) arr

spreadsheetEval :: Map NamedReference (CellValue TypeError) -> Array CellIndex (CellValue TypeError) -> Array CellIndex (Either TypeError (CellType, CellRawValue TypeError))
spreadsheetEval refMap arr = go where go = fmap ((`evalState` (M.empty, stdLib)) . runExceptT . evaluate (go !)) arr

exampleValues :: Array (Natural, Natural) (CellValue TypeError)
exampleValues = listArray ((0,0),(2,3))
  [ RawValue $ IntValue 1, RawValue $ IntValue 2, RawValue $ FloatValue 3.5, RawValue $ TypeValue IntType
  , RawValue $ IntValue 4, RawValue $ IntValue 5, RawValue $ ListValue (IntValue <$> [6,7,8]), RawValue $ IntValue 2
  , RawValue $ PosValue (1,2), App (App (NamedRefValue "Plus") (RawValue $ IntValue 8)) (RawValue $ IntValue 5), App (NamedRefValue "Sum") (Range (range ((0,0),(1,1)))), RawValue $ StringValue "1"]

exampleSheet :: Array (Natural, Natural) String
exampleSheet = listArray ((0,0),(2,4))
  [ "1", "2", "3.5", "#Int", "[6,7,8]"
  , "4", "5", "@{1,2}", "Plus 8 5", "\"1\""
  , "\\x:Pos => (Sum (RangeInt @{0,0} x))", "\\x:a => a", "\\x:a => \\y:b => x", "\\x:(a -> b -> c) => \\y:a -> b => \\z:a => (x z (y z))", "@!{2,3} @!{2,2} @!{2,2}"]

stdLib :: Map NamedReference (CellValue TypeError)
stdLib = M.fromList (basicIntFns ++ basicRangeFns)

