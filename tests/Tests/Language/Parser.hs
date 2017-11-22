{-# LANGUAGE OverloadedStrings #-}
module Tests.Language.Parser where
import Test.Tasty
import Spread.Parser
import Spread.TypesAndVals
import Morte.Core
import qualified Data.Text.Lazy as T
import qualified Test.Tasty.SmallCheck as SC
import Data.Monoid ((<>))

parserTests = testGroup "Parser tests" [intParse, floatParse, stringParse, sIsParsed, kIsParsed, parenthesesAndSpacePlayWell]

intParse = SC.testProperty "Parsing an int gives back the same int" $ \ a -> case exprFromText "" (T.pack $ show a) of
  Right (Embed (RawValue (IntValue b))) -> a == b
  _ -> False

floatParse = SC.testProperty "Parsing a float gives back the same float" $ \ a -> case exprFromText "" (T.pack $ show a) of
  Right (Embed (RawValue (FloatValue b))) -> a == b
  _ -> False

stringParse = SC.testProperty "Parsing a string gives back the same string" $ \ a -> case exprFromText "" (T.pack $ show a) of
  Right (Embed (RawValue (StringValue b))) -> T.pack a == b
  _ -> False

s' = "\\a:* -> \\b:* -> \\c:* -> \\x:(Π_:a -> Π_:b -> c) -> \\y:(Π_:a -> b) -> \\z:a -> (x z (y z))"
k' = "\\a:* -> \\b:* -> \\x:a -> \\y:b -> x"
es = exprFromText "" s'
ek = exprFromText "" k'

sIsParsed = SC.testProperty "Parse a well known combinator (1/2)" $ case es of
  Right _ -> True
  _ -> False

kIsParsed = SC.testProperty "Parse a well known combinator (2/2)" $ case ek of
  Right _ -> True
  _ -> False

parenthesesAndSpacePlayWell = SC.testProperty "An expression with parentheses and spaces is correctly parsed (Regression test)" $ \i -> case exprFromText "" ("(\\b:#Int -> b) " <> (T.pack $ show i)) of
  Right (App (Lam "b" (Embed (RawValue (TypeValue Int))) (Var (V "b" 0))) (Embed (RawValue (IntValue j)))) -> i == j
  _ -> False

