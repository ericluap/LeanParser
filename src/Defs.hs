{-
    This file has the basic definitions used throughout the parser.
-}
module Defs where

import Structures

type SyntaxNodeKind = String
type Token = String
type Name = String
type Error = String

numLitKind :: SyntaxNodeKind
numLitKind = "num"

identKind :: SyntaxNodeKind
identKind = "ident"

maxPrec :: Int
maxPrec = 1024

{-
    A term of type `Syntax` is the result of parsing.

    `Node` represents a grouping of syntax with its `SyntaxNodeKind`
    tracking different kinds of grouping.
    `Atom` represents a token that is a keyword.
    `Ident` represents a token that is an identifier.
-}
data Syntax = Missing
    | Node SyntaxNodeKind [Syntax]
    | Atom String
    | Ident String
    deriving (Show, Eq)

{-
    `syntax` stores the list of syntax we have created so far,
    often the top elemenst are replaced with a single node that has
    those elements as children (see `mkNode`)

    `pos` is the current position of the character we are parsing
    within the input string, it is incremented by `nextPos`

    `errorMsg` is the current error message, if it is set then operations
    will do nothing and just return the current state so that it propagates
    upwards

    `lhsPrec` stores the precedence level of the previous parser that ran,
    this is set by `setLhsPrec` which is called from any parser made by
    `leadingNode` or `trailingNode`
-}
data ParserState = ParserState {
    syntax :: [Syntax],
    pos :: Int,
    errorMsg :: Maybe Error,
    lhsPrec :: Int
}
    deriving (Show, Eq)

{-
    `prec` determines what parser are allowed to run
    (any parser that calls `checkPrec` will only run if its
    precedence is greater than or equal to this one)
    `inputString` is the entire string we are parsing
    `ctxTokens` stores the possible tokens we can parse
    `rules` stores all the parsing rules
    (the equivalent of `Parser.Extension.State` in Lean)
-}
data ParserContext = ParserContext {
    prec :: Int,
    inputString :: String,
    ctxTokens :: TokenTable
}

{-
    The core type of parsing functions.
    Given a parsing context and state, they produce an updated state.

    The updates change the position of the current state and add
    newly produced syntax to the state.
-}
type ParserFn = ParserContext -> ParserState -> ParserState

data ParserInfo = ParserInfo {
    -- Adds the tokens relevant for the parser
    collectTokens :: [Token] -> [Token],
    -- Adds the kinds relevant for the parser
    collectKinds :: [SyntaxNodeKind] -> [SyntaxNodeKind]
}

data Parser = Parser {
    info :: ParserInfo,
    fn :: ParserFn
}

getKind :: Syntax -> SyntaxNodeKind
getKind stx =
    case stx of
    Node kind _ -> kind
    Missing -> "missing"
    Atom val -> val
    Ident _ -> identKind

hasError :: ParserState -> Bool
hasError s =
    case errorMsg s of
    Nothing -> False
    Just _ -> True

{-
    Add the given syntax to the top of the syntax list in the parser state.
-}
pushSyntax :: ParserState -> Syntax -> ParserState
pushSyntax s n = s {syntax = n : syntax s}

popSyntax :: ParserState -> ParserState
popSyntax s =
    case syntax s of
    [] -> s
    _ : xs -> s {syntax = xs}

setError :: ParserState -> Error -> ParserState
setError s e = s { errorMsg = Just e }

{-
    Sets the error message and pushes `Missing`
    onto the syntax list.
-}
mkError :: ParserState -> Error -> ParserState
mkError s e = pushSyntax (setError s e) Missing

mkUnexpectedTokenError :: ParserState -> Error -> ParserState
mkUnexpectedTokenError s = mkError (popSyntax s)

mkEOIError :: ParserState -> ParserState
mkEOIError s = mkError s "unexpected end of input"

{-
    Sets the error message and moves the current position
    back to the given one.
-}
mkErrorAt :: ParserState -> Error -> Int -> ParserState
mkErrorAt s e resetPos = mkError (s {pos = resetPos}) e

type TokenTable = Trie Token

{-
    Check if the given position is past the end
    of the input string.
-}
atEnd :: ParserContext -> Int -> Bool
atEnd c p = p >= length (inputString c)

{-
    Get the character at index `p` from the input string
    in the `ParserContext`.
-}
getInputChar :: ParserContext -> Int -> Char
getInputChar c p = inputString c !! p


{-
    Increment the current position in the parser state
-}
nextPos :: ParserState -> ParserState
nextPos s = s { pos = pos s + 1}