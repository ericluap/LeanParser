module ParseSpec (spec) where

import Test.Hspec
import Defs
import Parse
import Parsers.Basic
import Parsers.Token
import Category
import Pratt.PrecParsers

identParser :: Parser
identParser = Parser {
    fn = identFn,
    info = ParserInfo {
        collectTokens = id,
        firstTokens = Unknown
    }
}

spec :: Spec
spec = do
    describe "reverseSyntax" $ do
        it "reverses nested Node children" $ do
            let start = Node "t" [Node "l" [Ident "hi", Atom "bye"], Atom "hi2"]
            let manual = Node "t" [Atom "hi2", Node "l" [Atom "bye", Ident "hi"]]
            let reversed = reverseSyntax start
            reversed `shouldBe` manual
    describe "parseAllCommands" $ do
        it "parses multiple commands" $ do
            let emptyContext = emptyParsingRules {
                inputString = "test := one\n test2 := two"
            }
            let cmdParser = leadingNode "def" 100
                    (identParser `andthen` symbol ":=" `andthen` identParser)
            let startContext = addLeadingParser "command" cmdParser emptyContext
            let initialState = ParserState {
                syntax = [],
                pos = 0,
                errorMsg = Nothing,
                lhsPrec = 0
            }
            let res = parseAllCommands startContext initialState
            let manualRes1 = (whitespace `andthenFn` fn cmdParser) startContext
                    initialState
            let manualRes2 = (whitespace `andthenFn` fn cmdParser) startContext
                    manualRes1
            res `shouldBe` manualRes2
    describe "parse" $ do
        it "parses multiple commands" $ do
            let cmdParser = leadingNode "def" 100 (identParser `andthen`
                    symbol ":=" `andthen` identParser)
            let parsingRules = addLeadingParser "command" cmdParser emptyParsingRules
            let res = parse "test := hi\n test2 := hi2" parsingRules
            let manual = [Node "def" [Ident "test", Atom ":=", Ident "hi"],
                    Node "def" [Ident "test2", Atom ":=", Ident "hi2"]]
            res `shouldBe` Right manual
            
