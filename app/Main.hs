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

{-
runParserCategory :: String -> Syntax
runParserCategory input =
    let p = whitespace `andthenFn` commandParser
    let ctx = mkParserContext input tokens-}

main :: IO ()
main = do
    putStrLn "hi"
