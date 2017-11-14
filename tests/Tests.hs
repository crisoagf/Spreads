{-# LANGUAGE OverloadedStrings #-}
import Test.Tasty
import Tests.Language.Parser
import Tests.Language.Morte
import Tests.Feature.Reference

main = defaultMain allTests

allTests = testGroup "All tests" [langTests, featureTests]

langTests = testGroup "Language tests" [parserTests, morteTests]

featureTests = testGroup "Feature tests" [refTests]

