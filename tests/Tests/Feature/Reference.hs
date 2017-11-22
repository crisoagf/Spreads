{-# LANGUAGE OverloadedStrings #-}
module Tests.Feature.Reference where
import Test.Tasty
import Spread
import Spread.Parser
import Spread.TypesAndVals
import Spread.Typecheck
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except
import Data.Array
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Test.Tasty.SmallCheck as SC

refTests = testGroup "References tests" [loopTests, outOfBoundsTests]

loopTests = testGroup "Loop tests" [selfRef, circular, goToCircular]

selfRefSheet, circularSheet, goToCircularSheet, oobTestSheet :: Array CellIndex Text

selfRefSheet = listArray ((CellIndex (0,0)),(CellIndex (0,0))) ["@!{0,0}"]

selfRef = localOption (mkTimeout 1000000) $ SC.testProperty "Self references do not loop forever, and error out" $ case spreadsheetEval (const Nothing) (spreadsheetParse selfRefSheet) ! (CellIndex (0,0)) of
  Left (RE (Loop [(CellIndex (0,0)),(CellIndex (0,0))])) -> True
  _ -> False

circularSheet = listArray ((CellIndex (0,0)),(CellIndex (0,1))) ["@!{0,1}","@!{0,0}"]

circular = localOption (mkTimeout 1000000) $ SC.testProperty "Self references do not loop forever, and error out" $ case spreadsheetEval (const Nothing) (spreadsheetParse circularSheet) ! (CellIndex (0,0)) of
  Left (RE (ErrorInRef (CellIndex (0,1)) (RE (Loop [(CellIndex (0,1)),(CellIndex (0,0)),(CellIndex (0,1))])))) -> True
  _ -> False

goToCircularSheet = listArray ((CellIndex (0,0)),(CellIndex (0,3))) ["@!{0,1}","@!{0,2}","@!{0,3}","@!{0,2}"]

goToCircular = localOption (mkTimeout 1000000) $ SC.testProperty "Self references do not loop forever, and error out" $ case spreadsheetEval (const Nothing) (spreadsheetParse goToCircularSheet) ! (CellIndex (0,0)) of
  Left (RE (ErrorInRef (CellIndex (0,1)) (RE (ErrorInRef (CellIndex (0,2)) (RE (ErrorInRef (CellIndex (0,3)) (RE (Loop [(CellIndex (0,3)),(CellIndex (0,2)),(CellIndex (0,3))])))))))) -> True
  _ -> False

outOfBoundsTests = testGroup "Out of bounds tests" [tooMuchFst, tooMuchSnd, negativeFst, negativeSnd]

oobTestSheet = listArray ((CellIndex (0,0)),(CellIndex (2,3)))
  ["@!{3,0}","@!{0,4}","@!{-1,2}","@{0,-4}",
   "@!{6,0}","@!{0,932810}","@!{-13083032803,2}","@{0,-2031312}",
   "@!{2983018309283,0}","@!{0,239803281938097421}","@!{-132017249721047910740701231,2}","@{0,-3183094927492187982}"]

oobResults = spreadsheetEval (const Nothing) (spreadsheetParse oobTestSheet) 

tooMuchFst = SC.testProperty "Excess first coordinate gracefully errors" $ case oobResults ! (CellIndex (0,0)) of 
  Left _ -> case oobResults ! (CellIndex (1,0)) of
    Left _ -> case oobResults ! (CellIndex (2,0)) of
      Left _ -> True
      _ -> False
    _ -> False
  _ -> False

tooMuchSnd = SC.testProperty "Excess second coordinate gracefully errors" $ case oobResults ! (CellIndex (0,1)) of 
  Left _ -> case oobResults ! (CellIndex (1,1)) of
    Left _ -> case oobResults ! (CellIndex (2,1)) of
      Left _ -> True
      _ -> False
    _ -> False
  _ -> False

negativeFst = SC.testProperty "Negative first coordinate gracefully errors" $ case oobResults ! (CellIndex (0,2)) of 
  Left _ -> case oobResults ! (CellIndex (1,2)) of
    Left _ -> case oobResults ! (CellIndex (2,2)) of
      Left _ -> True
      _ -> False
    _ -> False
  _ -> False

negativeSnd = SC.testProperty "Negative second coordinate gracefully errors" $ case oobResults ! (CellIndex (0,3)) of 
  Left _ -> case oobResults ! (CellIndex (1,3)) of
    Left _ -> case oobResults ! (CellIndex (2,3)) of
      Left _ -> True
      _ -> False
    _ -> False
  _ -> False

