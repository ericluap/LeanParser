module Main (main) where

import Trie

type SyntaxNodeKind = String
type Token = String

data Syntax = Missing
    | Node SyntaxNodeKind [Syntax]
    | Atom String

{-
    `pos` is the current position of the character we are parsing
    within the input string
-}
data ParserState = ParserState {
    syntax :: [Syntax],
    pos :: Int
}

type TokenTable = Trie Token
{-
    `inputString` is the entire string we are parsing
-}
data ParserContext = ParserContext {
    prec :: Int,
    inputString :: String,
    tokens :: TokenTable
}

{-
    Get the character at index `p` from the input string
    in the `ParserContext`.
-}
getInputChar :: ParserContext -> Int -> Char
getInputChar c p = (inputString c) !! p

type ParserFn = ParserContext -> ParserState -> ParserState


{-
tokenFn :: ParserFn
tokenFn c s =
    let i = pos s
        curr = getChar c i in
    let tk = (tokens c)-}

main :: IO ()
main = do
    let c = ParserContext {
        prec = 0,
        inputString = "name : Type",
        tokens = empty
    }
    let res = getInputChar c 3
    putStrLn (show res)

    let trie1 = empty
    let trie2 = (insert "def" 5 trie1)
    let trie3 = (insert "definition" 6 trie2)
    let match = matchPrefix "definitions" trie3 0
    putStrLn (show match)
