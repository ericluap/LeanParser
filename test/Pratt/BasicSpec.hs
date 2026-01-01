module Pratt.BasicSpec (spec) where

import Test.Hspec
import Defs
import Structures
import Parsers.Basic
import Pratt.PrecParsers
import Pratt.Basic
import Data.Map (Map)
import qualified Data.Map as Map

startContext :: ParserContext
startContext = ParserContext {
    prec = 0,
    inputString = "test := hi : Type",
    ctxTokens = insert ":=" ":=" (insert ":" ":" empty),
    categories = Map.empty
}

startState :: ParserState
startState = ParserState {
    syntax = [],
    pos = 0,
    errorMsg = Nothing,
    lhsPrec = 0
}

commandParser :: Parser
commandParser =
    leadingNode "def" 100 (ident `andthen` symbol ":=" `andthen` ident)

commandParserLong :: Parser
commandParserLong =
    leadingNode "def" 100 (ident `andthen` symbol ":=" `andthen` ident
    `andthen` symbol ":" `andthen` ident)

spec :: Spec
spec = do
    describe "longestMatchFn" $ do
        context "when given a single leading parser" $ do
            it "just runs that leading parser" $ do
                let justRunningRes = fn commandParser startContext startState
                let longestMatchRes = longestMatchFn Nothing [commandParser]
                        startContext startState
                longestMatchRes `shouldBe` justRunningRes
        context "when given a single non-lhsPrec setting parser" $ do
            it "runs that parser and sets the lhsPrec to maxPrec" $ do
                let justRunningRes = fn ident startContext startState
                let longestMatchRes = longestMatchFn Nothing [ident]
                        startContext startState
                syntax longestMatchRes `shouldBe` syntax justRunningRes
                pos longestMatchRes `shouldBe` pos justRunningRes
                lhsPrec longestMatchRes `shouldBe` maxPrec
        context "when given two matching parsers" $ do
            it "chooses the one with the longer match" $ do
                let firstMatchRes = fn commandParser startContext startState
                let secondMatchRes = fn commandParserLong startContext startState
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
            let manualRes = fn commandParserLong startContext startState
            let leadingRes = leadingParser "hi" testTables startContext startState
            leadingRes `shouldBe` manualRes
        it "uses the indexed leading parsers" $ do
            let startContext = ParserContext {
                    prec = 0,
                    inputString = ":= hi : Type",
                    ctxTokens = insert ":=" ":=" (insert ":" ":" empty),
                    categories = Map.empty
                }
            let defParser = leadingNode "eq" 100 (symbol ":=" `andthen` ident)
            let testTables = PrattParsingTables {
                    leadingTable = insertTokenMap emptyTokenMap ":=" defParser,
                    leadingParsers = [],
                    trailingTable = emptyTokenMap,
                    trailingParsers = []
                }
            let manualRes = fn defParser startContext startState
            let leadingRes = leadingParser "hi" testTables startContext startState
            leadingRes `shouldBe` manualRes
    describe "trailingLoop" $ do
        it "uses the non-indexed trailing parsers" $ do
            let trailingCmdParser = trailingNode "def" 100 100
                    (symbol ":=" `andthen` ident)
            let startContext = ParserContext {
                    prec = 0,
                    inputString = ":= hi",
                    ctxTokens = insert ":=" ":=" (insert ":" ":" empty),
                    categories = Map.empty
                }
            let trailingState = startState
                    {syntax = [Ident (SourceInfo "") "test"], lhsPrec = 100}
            let testTables = PrattParsingTables {
                leadingTable = emptyTokenMap,
                leadingParsers = [],
                trailingTable = emptyTokenMap,
                trailingParsers = [trailingCmdParser]
            }
            let manualRes = fn trailingCmdParser startContext trailingState
            let leadingRes = trailingLoop testTables startContext trailingState
            leadingRes `shouldBe` manualRes
        it "uses the indexed trailing parsers" $ do
            let trailingCmdParser = trailingNode "def" 100 100
                    (symbol ":=" `andthen` ident)
            let startContext = ParserContext {
                    prec = 0,
                    inputString = ":= hi",
                    ctxTokens = insert ":=" ":=" (insert ":" ":" empty),
                    categories = Map.empty
                }
            let trailingState = startState
                    {syntax = [Ident (SourceInfo "") "test"], lhsPrec = 100}
            let testTables = PrattParsingTables {
                leadingTable = emptyTokenMap,
                leadingParsers = [],
                trailingTable = insertTokenMap emptyTokenMap ":=" trailingCmdParser,
                trailingParsers = []
            }
            let manualRes = fn trailingCmdParser startContext trailingState
            let leadingRes = trailingLoop testTables startContext trailingState
            leadingRes `shouldBe` manualRes
        it "repeatedly matches trailing parsers" $ do
            let trailingCmdParser = trailingNode "def" 100 100
                    (symbol ":=" `andthen` ident)
            let startContext = ParserContext {
                    prec = 0,
                    inputString = ":= hi := hiagain",
                    ctxTokens = insert ":=" ":=" (insert ":" ":" empty),
                    categories = Map.empty
                }
            let trailingState = startState
                    {syntax = [Ident (SourceInfo "") "test"], lhsPrec = 100}
            let testTables = PrattParsingTables {
                leadingTable = emptyTokenMap,
                leadingParsers = [],
                trailingTable = insertTokenMap emptyTokenMap ":=" trailingCmdParser,
                trailingParsers = []
            }
            let manualRes1 = fn trailingCmdParser startContext trailingState
            let manualRes2 = fn trailingCmdParser startContext manualRes1
            let leadingRes = trailingLoop testTables startContext trailingState
            leadingRes `shouldBe` manualRes2
    describe "prattParser" $ do
        it "uses both leading and trailing parsers" $ do
            let trailingParser = trailingNode "type" 100 100
                    (symbol ":" `andthen` ident)
            let testTables = PrattParsingTables {
                    leadingTable = emptyTokenMap,
                    leadingParsers = [commandParser],
                    trailingTable = insertTokenMap emptyTokenMap ":" trailingParser,
                    trailingParsers = []
                }
            let manualRes1 = fn commandParser startContext startState
            let manualRes2 = fn trailingParser startContext manualRes1
            let leadingRes = prattParser "hi" testTables startContext startState
            leadingRes `shouldBe` manualRes2 