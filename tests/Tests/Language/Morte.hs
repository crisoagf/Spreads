{-# LANGUAGE OverloadedStrings #-}
module Tests.Language.Morte where
import Spread.TypesAndVals
import Spread.Typecheck

import qualified Data.Text.Lazy as T
import Morte.Core
import Test.Tasty
import qualified Test.Tasty.SmallCheck as SC
import Tests.Language.Parser (es, ek)

eskk = (\ s k -> Lam "a" (Const Star) (
                   Lam "b" (Const Star) (
                     App (
                       App (
                         App (App (App s (Var (V "a" 0))) (
                           Pi "" (Var (V "b" 0)) (Var (V "a" 0))
                         )) (Var (V "a" 0))
                       ) (
                         App (App k (Var (V "a" 0))) (
                           Pi "" (Var (V "b" 0)) (Var (V "a" 0))
                         )
                       )
                     ) (
                       App (App k (Var (V "a" 0))) (Var (V "b" 0))
                     )
                   )
                 )) <$> es <*> ek

morteTests = testGroup "Morte Integration tests" [skkIsId]

skkIsId = testGroup "S K K is the identity" [skkIsIdInt, skkIsIdFloat, skkIsIdString]

skkIsIdInt = SC.testProperty "Known fact: S K K is the identity! (for ints)" $ \ i ->
          case eskk of
            Left _ -> False
            Right skk -> 
              case runSome (EvalContext undefined undefined) (evaluate (CellIndex (0,0)) $ App (App (App skk (Embed $ RawValue $ TypeValue $ Int)
                ) (Embed $ RawValue $ TypeValue $ Int)
              ) (Embed $ RawValue $ IntValue i)) of
                Right (Embed (IntValue j)) -> i == j

skkIsIdFloat = SC.testProperty "Known fact: S K K is the identity! (for floats)" $ \ i ->
          case eskk of
            Left _ -> False
            Right skk -> 
              case runSome (EvalContext undefined undefined) (evaluate (CellIndex (0,0)) $ App (App (App skk (Embed $ RawValue $ TypeValue $ Float)
                ) (Embed $ RawValue $ TypeValue $ Int)
              ) (Embed $ RawValue $ FloatValue i)) of
                Right (Embed (FloatValue j)) -> i == j

skkIsIdString = SC.testProperty "Known fact: S K K is the identity! (for strings)" $ \ i ->
          case eskk of
            Left _ -> False
            Right skk -> 
              case runSome (EvalContext undefined undefined) (evaluate (CellIndex (0,0)) $ App (App (App skk (Embed $ RawValue $ TypeValue $ String)
                ) (Embed $ RawValue $ TypeValue $ Int)
              ) (Embed $ RawValue $ StringValue $ T.pack i)) of
                Right (Embed (StringValue j)) -> T.pack i == j

