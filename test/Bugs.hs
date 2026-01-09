{-
    Tests for bugs that have been fixed.
-}
module Bugs (spec) where

import Test.Hspec
import LeanParser
import Position
import qualified Data.Map as Map

spec :: Spec
spec = do
    describe "infinite loop in trailingLoop" $ do
        context "when no trailing parsers are applicable" $ do
            it "the trailingLoop ends" $ do
                let command = "command"
                let plus = trailingNode "plus" 65 65
                        (symbol "+" `andthen` categoryParser command 66)
                let mul = trailingNode "mul" 70 70
                        (symbol "*" `andthen` categoryParser command 71)
                let leadingCtx = addLeadingParser command num emptyParsingRules
                let rulesCtx = addTrailingParsers command [plus, mul] leadingCtx
                let res = parse "12 * 3 + 65" rulesCtx
                res `shouldBe` (Nothing, [Node "plus"
                    [Node "mul"
                        [Node "num" [Atom (SourceInfo "" 0) "12"],
                            Atom (SourceInfo "" 0) "*", Node "num" [Atom (SourceInfo "" 0) "3"]],
                    Atom (SourceInfo "" 0) "+", Node "num" [Atom (SourceInfo "" 0) "65"]]])
    describe "toPosition in FileMap" $ do
        it "gets correct column number for first line" $ do
            let input = "first\nsecond"
            let fileMap = fileMapOfString input
            let pos1 = toPosition fileMap 0
            let manual1 = Position {line = 0, column = 0}
            pos1 `shouldBe` manual1

            let pos2 = toPosition fileMap 6
            let manual2 = Position {line = 1, column = 0}
            pos2 `shouldBe` manual2
