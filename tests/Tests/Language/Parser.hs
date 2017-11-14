{-# LANGUAGE OverloadedStrings #-}
module Tests.Language.Parser where
import Test.Tasty
import Spread.Parser
import Spread.TypesAndVals
import Morte.Core
import qualified Data.Text.Lazy as T
import qualified Test.Tasty.SmallCheck as SC

parserTests = testGroup "Parser tests" [intParse, floatParse, stringParse, sIsParsed, kIsParsed]

intParse = SC.testProperty "Parsing an int gives back the same int" $ \ a -> case exprFromText "" (T.pack $ show a) of
  Right (Embed (RawValue (IntValue b))) -> a == b
  _ -> False

floatParse = SC.testProperty "Parsing a float gives back the same float" $ \ a -> case exprFromText "" (T.pack $ show a) of
  Right (Embed (RawValue (FloatValue b))) -> a == b
  _ -> False

stringParse = SC.testProperty "Parsing a string gives back the same string" $ \ a -> case exprFromText "" (T.pack $ show a) of
  Right (Embed (RawValue (StringValue b))) -> T.pack a == b
  _ -> False

s' = "\\a:* => \\b:* => \\c:* => \\x:(a -> b -> c) => \\y:(a -> b) => \\z:a => (x z (y z))"
k' = "\\a:* => \\b:* => \\x:a => \\y:b => x"
es = exprFromText "" s'
ek = exprFromText "" k'

sIsParsed = SC.testProperty "Parse a well known combinator (1/2)" $ case es of
  Right _ -> True
  _ -> False

kIsParsed = SC.testProperty "Parse a well known combinator (2/2)" $ case ek of
  Right _ -> True
  _ -> False

