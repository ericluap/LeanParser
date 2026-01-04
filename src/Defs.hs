{-
    This file has the basic definitions used throughout the parser.
-}
module Defs where

import Structures
import Position
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Foldable (asum)
import Data.Maybe (fromMaybe)

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
    Tracks information about the input string that was used
    to create a piece of `Syntax`.

    `trailing` tracks the trailing whitespace
    `startPos` stores the starting position within the input string
-}
data SourceInfo = SourceInfo {
    trailing :: String,
    startPos :: Int
}
    deriving (Show, Eq)

{-
    A term of type `Syntax` is the result of parsing.

    `Node` represents a grouping of syntax with its `SyntaxNodeKind`
    tracking different kinds of grouping.
    `Atom` represents a token that is a keyword.
    `Ident` represents a token that is an identifier.
-}
data Syntax = Missing
    | Node SyntaxNodeKind [Syntax]
    | Atom SourceInfo String
    | Ident SourceInfo String
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
    Stores the list of tokens that may appear at the start
    of the parser. This is used to index in the parser in the
    Pratt parsing tables.

    `Epsilon` means it has no first tokens and the first tokens of the
    following parser should be used
    `Unknown` means we do not know its first tokens and so must be
    nonindexed
    `Tokens` contains a list of tokens, one of which will appear first
    `OptTokens` contains a list of tokens that could be first
    but don't need to be.

    We need to distinguish between `Tokens` and `OptTokens`
    because they lead to different behavior when parses are combined.
    See the docs for `seqFirstTokens`.
-}
data FirstTokens =
    Epsilon
    | Unknown
    | Tokens (Set Token)
    | OptTokens (Set Token)

data ParserInfo = ParserInfo {
    -- Adds the tokens relevant for the parser, used to add them to
    -- the `ctxTokens` in the parser context
    collectTokens :: [Token] -> [Token],
    -- Starting tokens, used to index the parser in `PrattParsingTables`
    firstTokens :: FirstTokens
}

{-
    Stores all the information needed for Pratt parsing.

    `leadingTable` maps the name of a token to the list of leading parsers that
    we know ahead of time can parse it
    `leadingParsers` is a list of leading parsers that we always try
    regardless of the token

    And then the same for the trailing parsers.
-}
data PrattParsingTables = PrattParsingTables {
    leadingTable :: TokenMap Parser,
    leadingParsers :: [Parser],
    trailingTable :: TokenMap Parser,
    trailingParsers :: [Parser]
}

type ParserCategories = Map String PrattParsingTables

type TokenTable = Trie Token


data ParserContext = ParserContext {
    -- Determines what parser are allowed to run
    -- (any parser that calls `checkPrec` will only run if its
    -- precedence is greater than or equal to this one)
    prec :: Int,
    -- The entire string we are parsing
    inputString :: String,
    -- Stores the possible tokens we can parse (all reserved symbols)
    ctxTokens :: TokenTable,
    -- Stores all the parsing rules
    categories :: ParserCategories,
    -- Stores the locations of all newlines in the input string
    -- in order to determine the column of a given position
    fileMap :: FileMap,
    -- Stores an index into the input string. Used as reference by parsers
    -- like `checkColGt`.
    savedPos :: Maybe Int
}

{-
    The core type of parsing functions.
    Given a parsing context and state, they produce an updated state.

    The updates change the position of the current state and add
    newly produced syntax to the state.
-}
type ParserFn = ParserContext -> ParserState -> ParserState

data Parser = Parser {
    info :: ParserInfo,
    fn :: ParserFn
}

{-
    Sequence the first tokens of two parsers.
    Normally, it is just the first tokens of the first parser.
    But if the first tokens of the first parser are optional, then
    it is possible for the first tokens of the second parser to still
    be first tokens and so they are combined.
-}
seqFirstTokens :: FirstTokens -> FirstTokens -> FirstTokens
seqFirstTokens fst snd =
    case (fst, snd) of
    (Epsilon, tks) -> tks
    (OptTokens s1, OptTokens s2) -> OptTokens (Set.union s1 s2)
    (OptTokens s1, Tokens s2) -> OptTokens (Set.union s1 s2)
    (tks, _) -> tks

toOptional :: FirstTokens -> FirstTokens
toOptional fst =
    case fst of
    Tokens tks -> OptTokens tks
    tks -> tks

{-
    Get the most recently parsed `SourceInfo`.
-}
getTailInfoMaybe :: Syntax -> Maybe SourceInfo
getTailInfoMaybe stx =
    case stx of
    Atom info _ -> Just info
    Ident info _ -> Just info
    Node _ children -> asum $ map getTailInfoMaybe children
    _ -> Nothing

{-
    Get the oldest parsed `SourceInfo`.
-}
getHeadInfoMaybe :: Syntax -> Maybe SourceInfo
getHeadInfoMaybe stx =
    case stx of
    Atom info _ -> Just info
    Ident info _ -> Just info
    Node _ children -> asum . reverse $ map getHeadInfoMaybe children
    _ -> Nothing

getPosMaybe :: Syntax -> Maybe Int
getPosMaybe stx = startPos <$> getHeadInfoMaybe stx

getKind :: Syntax -> SyntaxNodeKind
getKind stx =
    case stx of
    Node kind _ -> kind
    Missing -> "missing"
    Atom _ val -> val
    Ident _ _ -> identKind

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

getTopSyntax :: ParserState -> Maybe Syntax
getTopSyntax s =
    case syntax s of
    [] -> Nothing
    x : _ -> Just x

setError :: ParserState -> Error -> ParserState
setError s e = s { errorMsg = Just e }

{-
    Sets the error message and pushes `Missing`
    onto the syntax list.
-}
mkError :: ParserState -> Error -> ParserState
mkError s e = pushSyntax (setError s e) Missing

{-
    Creates an error, pops the token, and resets the position
    to be before the token was added.

    Resetting the position is important as it makes this error
    not consume input which is used by parser combinators like
    `optional`.
-}
mkUnexpectedTokenError :: ParserState -> Error -> ParserState
mkUnexpectedTokenError s error =
    let newPos = case getTopSyntax s of
            Nothing -> 0
            Just tk -> fromMaybe 0 (getPosMaybe tk)
        new_s = s {pos = newPos} in
    mkError (popSyntax new_s) error

mkEOIError :: ParserState -> ParserState
mkEOIError s = mkError s "unexpected end of input"

{-
    Sets the error message and moves the current position
    back to the given one.
-}
mkErrorAt :: ParserState -> Error -> Int -> ParserState
mkErrorAt s e resetPos = mkError (s {pos = resetPos}) e

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

{-
    An initial empty parsing context to be used for adding
    leading and trailing parsers to.
-}
emptyParsingRules :: ParserContext
emptyParsingRules = ParserContext {
    prec = 0,
    inputString = "",
    ctxTokens = empty,
    categories = Map.empty,
    fileMap = fileMapOfString "",
    savedPos = Nothing
}

{-
    An initial empty Pratt parsing tables to be used for
    newly created parsing categories.
-}
emptyParsingTables :: PrattParsingTables
emptyParsingTables = PrattParsingTables {
    leadingTable = emptyTokenMap,
    leadingParsers = [],
    trailingTable = emptyTokenMap,
    trailingParsers = []
}

{-
    Display the given syntax with parentheses inserted for `Node`
    and the `Node` syntax kind specified.
-}
withParentheses :: Syntax -> String
withParentheses stx = go "" stx
    where
        go :: String -> Syntax -> String
        go preTrail stx =
            case stx of
            Missing -> "Missing"
            Atom (SourceInfo trailing _) val -> val ++ preTrail ++ trailing
            Ident (SourceInfo trailing _) val -> val ++ preTrail ++ trailing
            Node kind children ->
                let childStr = concatMap (go "") (init children)
                    lastChild = go (preTrail ++ ")") (last children) in
                "(" ++ kind ++ "| " ++ childStr ++ lastChild