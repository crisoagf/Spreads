{-# LANGUAGE OverloadedStrings #-}
module Tests.Feature.Reference where
import Test.Tasty
import Spread.Parser
import Spread.TypesAndVals
import Spread.Typecheck
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except
import Data.Array
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Test.Tasty.SmallCheck as SC

spreadsheetParse :: Array CellIndex (Either CellError Text) -> Array CellIndex (Either CellError CellExpr)
spreadsheetParse = fmap (exprFromText "input" =<<)

spreadsheetEval :: (NamedReference -> Maybe CellRawExpr) -> Array CellIndex (Either CellError CellExpr) -> Array CellIndex (Either CellError CellWithType)
spreadsheetEval refMap arr = go where go = fmap (((`runReader` (EvalContext (either typeAndEvaluate (ExceptT . pure . (go !))) refMap)) . runExceptT . typeAndEvaluate) =<<) arr

refTests = testGroup "References tests" [loopTests, outOfBoundsTests]

loopTests = testGroup "Loop tests" [selfRef, circular]

selfRefSheet = listArray ((0,0),(0,0)) ["@!{0,0}"]

selfRef = localOption (mkTimeout 1000000) $ SC.testProperty "Self references do not loop forever, and error out" $ case spreadsheetEval (const Nothing) (spreadsheetParse $ fmap Right selfRefSheet) ! (0,0) of
  Left (RE (Loop [(0,0),(0,0)])) -> True
  _ -> False

circularSheet = listArray ((0,0),(0,1)) ["@!{0,1}","@!{0,0}"]

circular = localOption (mkTimeout 1000000) $ SC.testProperty "Self references do not loop forever, and error out" $ case spreadsheetEval (const Nothing) (spreadsheetParse $ fmap Right circularSheet) ! (0,0) of
  Left (RE (Loop [(0,0),(0,1),(0,0)])) -> True
  _ -> False

outOfBoundsTests = testGroup "Out of bounds tests" [tooMuchFst, tooMuchSnd, negativeFst, negativeSnd]

oobTestSheet = listArray ((0,0),(2,3))
  ["@!{3,0}","@!{0,4}","@!{-1,2}","@{0,-4}",
   "@!{6,0}","@!{0,932810}","@!{-13083032803,2}","@{0,-2031312}",
   "@!{2983018309283,0}","@!{0,239803281938097421}","@!{-132017249721047910740701231,2}","@{0,-3183094927492187982}"]

oobResults = spreadsheetEval (const Nothing) (spreadsheetParse $ fmap Right oobTestSheet) 

tooMuchFst = SC.testProperty "Excess first coordinate gracefully errors" $ case oobResults ! (0,0) of 
  Left _ -> case oobResults ! (1,0) of
    Left _ -> case oobResults ! (2,0) of
      Left _ -> True
      _ -> False
    _ -> False
  _ -> False

tooMuchSnd = SC.testProperty "Excess second coordinate gracefully errors" $ case oobResults ! (0,1) of 
  Left _ -> case oobResults ! (1,1) of
    Left _ -> case oobResults ! (2,1) of
      Left _ -> True
      _ -> False
    _ -> False
  _ -> False

negativeFst = SC.testProperty "Negative first coordinate gracefully errors" $ case oobResults ! (0,2) of 
  Left _ -> case oobResults ! (1,2) of
    Left _ -> case oobResults ! (2,2) of
      Left _ -> True
      _ -> False
    _ -> False
  _ -> False

negativeSnd = SC.testProperty "Negative second coordinate gracefully errors" $ case oobResults ! (0,3) of 
  Left _ -> case oobResults ! (1,3) of
    Left _ -> case oobResults ! (2,3) of
      Left _ -> True
      _ -> False
    _ -> False
  _ -> False

