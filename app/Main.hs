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
    pos :: Int,
    errorMsg:: Maybe String
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

atEnd :: ParserContext -> Int -> Bool
atEnd c p = p >= length (inputString c)

{-
    Get the character at index `p` from the input string
    in the `ParserContext`.
-}
getInputChar :: ParserContext -> Int -> Char
getInputChar c p = (inputString c) !! p


{-
    Increment the current position in the parser state
-}
nextPos :: ParserState -> ParserState
nextPos s = s { pos = (pos s) + 1}

type ParserFn = ParserContext -> ParserState -> ParserState

takeUntilFn :: (Char -> Bool) -> ParserFn
takeUntilFn p c s =
    let i = (pos s) in
    if atEnd c i || p (getInputChar c i) then
        s
    else
        takeUntilFn p c (nextPos s)


{-
tokenFn :: ParserFn
tokenFn c s =
    let i = pos s
        curr = getInputChar c i
        tk = matchPrefix (inputString c) (tokens c) i in
    --identFn i tk c s
    s-}

main :: IO ()
main = do
    let c = ParserContext {
        prec = 0,
        inputString = "name : Type",
        tokens = empty
    }
    let s = ParserState {
        syntax = [],
        pos = 0,
        errorMsg = Nothing
    }
    let res = takeUntilFn (\x -> x == ':') c s
    putStrLn (show $ pos res)

    let trie1 = empty
    let trie2 = (insert "def" 5 trie1)
    let trie3 = (insert "definition" 6 trie2)
    let match = matchPrefix "definitions" trie3 0
    putStrLn (show match)
