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
            let manual = Node numLitKind [Atom (SourceInfo " ") "123"]
            syntax res `shouldBe` [manual]
    describe "checkLinebreakBefore" $ do
        it "errors if there is not line break" $ do
            let startContext = emptyParsingRules {
                inputString = "123 hi"
            }
            let startState = ParserState {
                syntax = [],
                pos = 0,
                errorMsg = Nothing,
                lhsPrec = 0
            }
            let step = fn num startContext startState
            let res = checkLinebreakBeforeFn startContext step
            let manual = Just "line break"
            errorMsg res `shouldBe` manual
        it "succeeds if there is a line break" $ do
            let startContext = emptyParsingRules {
                inputString = "123 \n hi"
            }
            let startState = ParserState {
                syntax = [],
                pos = 0,
                errorMsg = Nothing,
                lhsPrec = 0
            }
            let step = fn num startContext startState
            let res = checkLinebreakBeforeFn startContext step
            let manual = Nothing
            errorMsg res `shouldBe` manual
