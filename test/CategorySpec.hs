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
        firstTokens = Epsilon
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
            let res = addLeadingParser "command" indexedParser startContext
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
            let res = addLeadingParser "command" indexedParser startContext
            let manualCtx = startContext {ctxTokens =
                insert "test" "test" (ctxTokens startContext)} 
            ctxTokens res `shouldBe` ctxTokens manualCtx
        it "adds non-indexed parser" $ do
            let nonindexedParser = identParser
            let res = addLeadingParser "command" nonindexedParser startContext
            let manualTable = commandTable {leadingParsers =
                nonindexedParser : leadingParsers commandTable}
            case Map.lookup "command" (categories res) of
                Nothing -> expectationFailure "command is not a valid category" 
                Just tables ->
                    length (leadingParsers tables) `shouldBe`
                        length (leadingParsers manualTable)