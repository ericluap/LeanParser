module ParseSpec (spec) where

import Test.Hspec
import Defs
import Parse
import Parsers.Basic
import Parsers.Token
import Category
import Pratt.PrecParsers

spec :: Spec
spec = do
    describe "reverseSyntax" $ do
        it "reverses nested Node children" $ do
            let start = Node "t" [Node "l" [Ident (SourceInfo "" 0) "hi",
                        Atom (SourceInfo "" 0) "bye"], Atom (SourceInfo "" 0) "hi2"]
            let manual = Node "t" [Atom (SourceInfo "" 0) "hi2",
                        Node "l" [Atom (SourceInfo "" 0) "bye",
                                Ident (SourceInfo "" 0) "hi"]]
            let reversed = reverseSyntax start
            reversed `shouldBe` manual
    describe "parseAllCommands" $ do
        it "parses multiple commands" $ do
            let emptyContext = emptyParsingRules {
                inputString = "test := one\n test2 := two"
            }
            let cmdParser = leadingNode "def" 100
                    (ident `andthen` symbol ":=" `andthen` ident)
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
            let cmdParser = leadingNode "def" 100 (ident `andthen`
                    symbol ":=" `andthen` ident)
            let parsingRules = addLeadingParser "command" cmdParser emptyParsingRules
            let res = parse "test := hi\n test2 := hi2" parsingRules
            let manual = [
                        Node "def" [Ident (SourceInfo " " 0) "test",
                                Atom (SourceInfo " " 5) ":=",
                                Ident (SourceInfo "\n " 8) "hi"
                        ],
                        Node "def" [ Ident (SourceInfo " " 12) "test2",
                                Atom (SourceInfo " " 18) ":=",
                                Ident (SourceInfo "" 21) "hi2"]]
            res `shouldBe` (Nothing, manual)
            
