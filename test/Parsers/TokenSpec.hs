module Parsers.TokenSpec (spec) where

import Test.Hspec
import Defs
import Parsers.Token

spec :: Spec
spec = do
    describe "tokenFn" $ do
        it "parses numbers" $ do
            let startContext = emptyParsingRules {
                inputString = "123 hi"
            }
            let startState = ParserState {
                syntax = [],
                pos = 0,
                errorMsg = Nothing,
                lhsPrec = 0
            }
            let res = tokenFn startContext startState
            let manual = Node numLitKind [Atom (SourceInfo " ") "123"]
            syntax res `shouldBe` [manual]