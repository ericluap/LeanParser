module Pratt.BasicSpec (spec) where

import Test.Hspec
import Defs
import Structures
import Parsers.Basic
import Pratt.PrecParsers
import Pratt.Basic

startContext :: ParserContext
startContext = ParserContext {
    prec = 0,
    inputString = "test := hi : Type",
    ctxTokens = insert ":=" ":=" (insert ":" ":" empty)
}

startState :: ParserState
startState = ParserState {
    syntax = [],
    pos = 0,
    errorMsg = Nothing,
    lhsPrec = 0
}

identParser :: Parser
identParser = Parser {
    fn = identFn,
    info = ParserInfo {
        collectTokens = \x -> x,
        collectKinds = \x -> x 
    }
}

commandParser :: Parser
commandParser =
    leadingNode "def" 100 (identParser `andthen` (symbol ":=") `andthen` identParser)

commandParserLong :: Parser
commandParserLong =
    leadingNode "def" 100 (identParser `andthen` (symbol ":=") `andthen` identParser
    `andthen` (symbol ":") `andthen` identParser)

spec :: Spec
spec = do
    describe "longestMatchFn" $ do
        context "when given a single leading parser" $ do
            it "just runs that leading parser" $ do
                let justRunningRes = (fn commandParser) startContext startState
                let longestMatchRes = longestMatchFn Nothing [commandParser]
                        startContext startState
                longestMatchRes `shouldBe` justRunningRes
        context "when given a single non-lhsPrec setting parser" $ do
            it "runs that parser and sets the lhsPrec to maxPrec" $ do
                let justRunningRes = (fn identParser) startContext startState
                let longestMatchRes = longestMatchFn Nothing [identParser]
                        startContext startState
                syntax longestMatchRes `shouldBe` syntax justRunningRes
                pos longestMatchRes `shouldBe` pos justRunningRes
                lhsPrec longestMatchRes `shouldBe` maxPrec
        context "when given two matching parsers" $ do
            it "chooses the one with the longer match" $ do
                let firstMatchRes = (fn commandParser) startContext startState
                let secondMatchRes = (fn commandParserLong) startContext startState
                let longerRes =
                        if pos firstMatchRes < pos secondMatchRes then
                            secondMatchRes
                        else
                            firstMatchRes
                let longestMatchRes = longestMatchFn Nothing
                        [commandParser, commandParserLong] startContext startState
                longestMatchRes `shouldBe` longerRes
    describe "leadingParser" $ do
        it "uses the non-indexed leading parsers" $ do
            let testTables = PrattParsingTables {
                leadingTable = emptyTokenMap,
                leadingParsers = [commandParser, commandParserLong],
                trailingTable = emptyTokenMap,
                trailingParsers = []
            }
            let manualRes = (fn commandParserLong) startContext startState
            let leadingRes = leadingParser "hi" testTables startContext startState
            leadingRes `shouldBe` manualRes