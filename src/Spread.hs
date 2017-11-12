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
import Control.Arrow hiding (first, second)
import Control.Monad
import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Data.Bifunctor
import Morte.Core
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO
import Text.Parsec

data Visions = Input | Value | Presentation

spreadsheetInferTypes :: (NamedReference -> Maybe CellRawExpr) -> Array CellIndex CellExpr -> Array CellIndex (Either TypeError CellType)
spreadsheetInferTypes refMap arr = fmap (join . fmap ((`runReader` (InferContext (either (fmap snd . getType) (ExceptT . pure . fmap snd . (go !))) refMap)) . runExceptT . uncurry returnTypes)) $ go where go = fmap ((`runReader` (InferContext (either (fmap snd . getType) (ExceptT . pure . fmap snd . (go !))) refMap)) . runExceptT . getType) arr

spreadsheetEval :: (NamedReference -> Maybe CellRawExpr) -> Array CellIndex CellExpr -> Array CellIndex (Either TypeError CellWithType)
spreadsheetEval refMap arr = go where go = fmap ((`runReader` (EvalContext (either typeAndEvaluate (ExceptT . pure . (go !))) refMap)) . runExceptT . typeAndEvaluate) arr

exampleSheet :: Array (Natural, Natural) String
exampleSheet = listArray ((0,0),(2,4))
  [ "1", "2", "3.5", "#Int", "[6,7,8:Int]"
  , "4", "5", "@{1,2}", "Plus 8 5", "\"1\""
  , "\\x:Pos => (Sum (Range #Int @{0,0} x))", "@!{2,0} @{1,1}", "\\a:* => \\b:* => \\x:a => \\y:b => x", "\\a:* => \\b:* => \\c:* => \\x:(a -> b -> c) => \\y:(a -> b) => \\z:a => (x z (y z))", "\\a:* => (\\b:* => (@!{2,3} a #(b -> a) a (@!{2,2} a #(b -> a)) (@!{2,2} a b)))"]

exampleSheetVals :: Either ParseError (Array CellIndex CellExpr)
exampleSheetVals = traverse (parse parseCell "input") exampleSheet

exampleSheetTypes :: Either (Either ParseError TypeError) (Array CellIndex CellType)
exampleSheetTypes = either (Left . Left) (first Right) $ (sequence . spreadsheetInferTypes stdLib) <$> exampleSheetVals

exampleSheetEval :: Either (Either ParseError TypeError) (Array CellIndex CellWithType)
exampleSheetEval = either (Left . Left) (first Right) $ (sequence . spreadsheetEval stdLib) <$> exampleSheetVals

stdLib :: NamedReference -> Maybe CellRawExpr
stdLib = flip lookup (basicIntFns ++ basicRangeFns)

example :: Array (Natural, Natural) CellExpr
example = listArray ((0,0),(2,2))
  [Embed $ RawValue $ IntValue 1, Embed $ RawValue $ IntValue 2, App (Embed (NamedRefValue "Plus")) (Embed (RawValue $ IntValue 5)),
   Embed $ RawValue $ IntValue 4, Embed $ RawValue $ IntValue 5, Embed $ RawValue $ PosValue (1,2),
   App (Embed $ RefValue (0,2)) (Embed $ RawValue $ IntValue 3), App (App (App (Embed (NamedRefValue "Range")) (Embed $ RawValue $ TypeValue (Embed Int))) (Embed (RawValue $ PosValue (0,0)))) (Embed $ RawValue $ PosValue (1,1)), App (App (App (Embed (NamedRefValue "Range")) (Embed $ RawValue $ TypeValue (Embed Float))) (Embed (RawValue $ PosValue (0,0)))) (Embed $ RawValue $ PosValue (1,1))]

exampleTypes = spreadsheetInferTypes stdLib example
exampleEval = spreadsheetEval stdLib example

showExample = mapM_ (TIO.putStrLn . pretty) example
showExampleTypes = mapM_ (either (TIO.putStrLn . pretty) (TIO.putStrLn . pretty)) exampleTypes
showExampleEval = mapM_ (either (TIO.putStrLn . pretty) (TIO.putStrLn . pretty)) exampleEval

main :: IO ()
main = putStrLn "Under construction!"

