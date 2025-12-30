module Main (main) where

import Defs
import Parsers.Token
import Parsers.Basic
import Pratt.PrecParsers
import Structures
import Pratt.Basic
import Defs

import Data.Map (Map)
import qualified Data.Map as Map

mkParserContext :: String -> TokenTable -> ParserContext
mkParserContext input tokens = ParserContext {
    prec = 0,
    inputString = input,
    ctxTokens = tokens
}

commandParser :: ParserFn
commandParser =
    checkLhsPrecFn 10 `andthenFn` trailingNodeFn "def" ((symbolFn ":=") `andthenFn` identFn)

{-
runParserCategory :: String -> Syntax
runParserCategory input =
    let p = whitespace `andthenFn` commandParser
    let ctx = mkParserContext input tokens-}

main :: IO ()
main = do
    let c = ParserContext {
        prec = 0,
        inputString = ":= hi test : Type",
        ctxTokens = insert ":=" ":=" (insert "name" "name" empty)
    }
    let s = ParserState {
        syntax = [],
        pos = 0,
        errorMsg = Nothing,
        lhsPrec = 0
    }

    let res = runLongestMatchParser (Just $ Ident "name") 10 commandParser c s
    putStrLn (show $ syntax res)
    putStrLn (show $ errorMsg res)


