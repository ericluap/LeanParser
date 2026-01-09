module PositionSpec (spec) where

import Position
import Test.Hspec

spec :: Spec
spec = do
    describe "fileMapOfString" $ do
        it "finds all the correct positions" $ do
            let str = "test\n this\n to see\n"
            let res = fileMapOfString str
            let manual = FileMap {
                source = str,
                positions = [0,5,11,19,19]
            }
            res `shouldBe` manual
    describe "toPosition" $ do
        it "correctly converts index to position" $ do
            let str = "test\n this\n to see\n"
            let fileMap = fileMapOfString str
            let res = toPosition fileMap 8
            let manual = Position {
                line = 1,
                column = 3
            }
            res `shouldBe` manual