module Parsers.BasicSpec (spec) where

import Test.Hspec
import Defs
import Parsers.Basic

spec :: Spec
spec = do
    describe "num" $ do
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
            let res = fn num startContext startState
            let manual = Node numLitKind [Atom "123"]
            syntax res `shouldBe` [manual]
