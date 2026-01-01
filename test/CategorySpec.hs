module CategorySpec (spec) where

import Test.Hspec
import Defs
import Structures
import Parsers.Basic
import Pratt.PrecParsers
import Pratt.Basic
import Category
import Data.Map (Map)
import qualified Data.Map as Map

identParser :: Parser
identParser = Parser {
    fn = identFn,
    info = ParserInfo {
        collectTokens = id,
        firstTokens = Unknown
    }
}

commandParser :: Parser
commandParser =
    leadingNode "def" 100 (identParser `andthen` symbol ":=" `andthen` identParser)

commandCategoryParser :: ParserFn
commandCategoryParser = categoryParser "command"

commandTable :: PrattParsingTables
commandTable = PrattParsingTables {
    leadingTable = emptyTokenMap,
    leadingParsers = [commandParser],
    trailingTable = emptyTokenMap,
    trailingParsers = []
}

startContext :: ParserContext
startContext = ParserContext {
    prec = 0,
    inputString = "test := hi : Type",
    ctxTokens = insert ":=" ":=" (insert ":" ":" empty),
    categories = Map.insert "command" commandTable Map.empty
}

startState :: ParserState
startState = ParserState {
    syntax = [],
    pos = 0,
    errorMsg = Nothing,
    lhsPrec = 0
}

spec :: Spec
spec = do
    describe "categoryParser" $ do
        it "calls prattParser for the given category" $ do
            let commandCategoryParser = categoryParser "command"
            let categoryParserRes = commandCategoryParser startContext startState
            let prattParserRes = prattParser "command" commandTable
                    startContext startState
            categoryParserRes `shouldBe` prattParserRes
    describe "addParserTokens" $ do
        it "adds all the used tokens to the context" $ do
            let startContext = ParserContext {
                    prec = 0,
                    inputString = "",
                    ctxTokens = empty,
                    categories = Map.empty
                }
            let addedCtx = addParserTokens commandParser startContext
            let manualCtx = startContext {ctxTokens = 
                    insert ":=" ":=" empty
                }
            ctxTokens addedCtx `shouldBe` ctxTokens manualCtx
    describe "addLeadingParser" $ do
        it "adds indexed parser" $ do
            let indexedParser = symbol "test"
            let Right res = addLeadingParser "command" indexedParser startContext
            let manualTable = commandTable {leadingTable =
                insertTokenMap (leadingTable commandTable) "test" indexedParser}
            case Map.lookup "command" (categories res) of
                Nothing -> expectationFailure "command is not a valid category" 
                Just tables ->
                    case (leadingTable tables, leadingTable manualTable) of
                    (TokenMap leadingTokenMap, TokenMap manualTokenMap) ->
                        Map.keys leadingTokenMap `shouldBe` Map.keys manualTokenMap
        it "adds the tokens" $ do
            let indexedParser = symbol "test"
            let Right res = addLeadingParser "command" indexedParser startContext
            let manualCtx = startContext {ctxTokens =
                insert "test" "test" (ctxTokens startContext)} 
            ctxTokens res `shouldBe` ctxTokens manualCtx
        it "adds non-indexed parser" $ do
            let nonindexedParser = commandParser
            let Right res = addLeadingParser "command" nonindexedParser startContext
            let manualTable = commandTable {leadingParsers =
                nonindexedParser : leadingParsers commandTable}
            case Map.lookup "command" (categories res) of
                Nothing -> expectationFailure "command is not a valid category" 
                Just tables ->
                    length (leadingParsers tables) `shouldBe`
                        length (leadingParsers manualTable)
        context "when a leading parser is added" $ do
            it "uses the leading parser" $ do
                let commandTable = PrattParsingTables {
                    leadingTable = emptyTokenMap,
                    leadingParsers = [],
                    trailingTable = emptyTokenMap,
                    trailingParsers = []
                }
                let startContext = ParserContext {
                    prec = 0,
                    inputString = "test := hi : Type",
                    ctxTokens = empty,
                    categories = Map.singleton "command" commandTable
                } 
                let Right newContext = addLeadingParser "command" commandParser startContext
                let parseRes = categoryParser "command" newContext startState
                let manualRes = fn commandParser newContext startState
                parseRes `shouldBe` manualRes
    describe "addTrailingParser" $ do
        it "adds indexed parser" $ do
            let indexedParser = symbol "test"
            let Right res = addTrailingParser "command" indexedParser startContext
            let manualTable = commandTable {trailingTable =
                insertTokenMap (trailingTable commandTable) "test" indexedParser}
            case Map.lookup "command" (categories res) of
                Nothing -> expectationFailure "command is not a valid category" 
                Just tables ->
                    case (trailingTable tables, trailingTable manualTable) of
                    (TokenMap trailingTokenMap, TokenMap manualTokenMap) ->
                        Map.keys trailingTokenMap `shouldBe` Map.keys manualTokenMap
        it "adds the tokens" $ do
            let indexedParser = symbol "test"
            let Right res = addTrailingParser "command" indexedParser startContext
            let manualCtx = startContext {ctxTokens =
                insert "test" "test" (ctxTokens startContext)} 
            ctxTokens res `shouldBe` ctxTokens manualCtx
        it "adds non-indexed parser" $ do
            let nonindexedParser = commandParser
            let Right res = addTrailingParser "command" nonindexedParser startContext
            let manualTable = commandTable {trailingParsers =
                nonindexedParser : trailingParsers commandTable}
            case Map.lookup "command" (categories res) of
                Nothing -> expectationFailure "command is not a valid category" 
                Just tables ->
                    length (trailingParsers tables) `shouldBe`
                        length (trailingParsers manualTable)
    describe "addLeadingParser & addTrailingParser" $ do
        context "when both a leading and trailing parser are added" $ do
            it "uses both to parse" $ do
                let commandTable = PrattParsingTables {
                    leadingTable = emptyTokenMap,
                    leadingParsers = [],
                    trailingTable = emptyTokenMap,
                    trailingParsers = []
                }
                let startContext = ParserContext {
                    prec = 0,
                    inputString = "test := hi : Type",
                    ctxTokens = empty,
                    categories = Map.singleton "command" commandTable
                } 
                let Right leadingContext = addLeadingParser "command" commandParser
                        startContext
                let typeParser = trailingNode "type" 100 100
                        (symbol ":" `andthen` identParser)
                let Right finalContext = addTrailingParser "command" typeParser
                        leadingContext
                let parseRes = categoryParser "command" finalContext startState
                let manualRes1 = fn commandParser finalContext startState
                let manualRes2 = fn typeParser finalContext manualRes1
                parseRes `shouldBe` manualRes2

