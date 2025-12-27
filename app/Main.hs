{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Defs
import Parsers.Token
import Parsers.Basic
import Trie
import Data.Map (Map)
import qualified Data.Map as Map

data State = State {
    tokens :: TokenTable,
    kinds :: [SyntaxNodeKind]
}

mkParserContext :: String -> TokenTable -> ParserContext
mkParserContext input tokens = ParserContext {
    prec = 0,
    inputString = input,
    ctxTokens = tokens
}

{-
    Checks that the given precedence is larger
    than the that in the context.
-}
checkPrecFn :: Int -> ParserFn
checkPrecFn precInt c s =
    if prec c <= precInt then
        s
    else
        mkError s "unexpected token at precedence level"

checkPrec :: Int -> Parser
checkPrec prec = Parser {
    info = ParserInfo {
        collectTokens = \x -> x,
        collectKinds = \x -> x
    },
    fn = checkPrecFn prec
}

{-
    Convert the top elements of syntax in the parser state
    into a single syntax node.
-}
mkNode :: ParserState -> SyntaxNodeKind -> Int -> ParserState
mkNode s k initialSize =
    let currentSize = length (syntax s) in
    {-
        If there is an error but no node has been added,
        add a missing node.
    -}
    if hasError s && currentSize == initialSize then
        pushSyntax s Missing
    else
        let lengthDiff = currentSize - initialSize
            children = extractSublist (syntax s) 0 lengthDiff
            newNode = Node k children
            syntaxListWithoutChildren = drop lengthDiff (syntax s)
            newSyntaxList = newNode : syntaxListWithoutChildren in
        s {syntax = newSyntaxList}

{-
    Convert the top elements (plus one!) of syntax in the parser state
    into a single syntax node.
-}
mkTrailingNode :: ParserState -> SyntaxNodeKind -> Int -> ParserState
mkTrailingNode s k initialSize =
    let currentSize = length (syntax s) + 1
        lengthDiff = currentSize - initialSize
        children = extractSublist (syntax s) 0 lengthDiff
        newNode = Node k children
        syntaxListWithoutChildren = drop lengthDiff (syntax s)
        newSyntaxList = newNode : syntaxListWithoutChildren in
    s {syntax = newSyntaxList} 

{-
    Runs the given parser and then puts all the
    syntax it created into a single node.
-}
nodeFn :: SyntaxNodeKind -> ParserFn -> ParserFn
nodeFn kind p c s =
    let initialSize = length (syntax s)    
        new_s = p c s in
    mkNode new_s kind initialSize

nodeInfo :: SyntaxNodeKind -> ParserInfo -> ParserInfo
nodeInfo kind p = ParserInfo {
    collectTokens = (collectTokens p),
    collectKinds = \s -> kind : (collectKinds p s)
}

{-
    Runs the parser and then collects the resulting
    syntax into a single node.
-}
node :: SyntaxNodeKind -> Parser -> Parser
node kind p = Parser {
    fn = nodeFn kind (fn p),
    info = nodeInfo kind (info p)
}

{-
    Runs the parser and then collects the resulting
    syntax as well as one more into a single node.
-}
trailingNodeFn :: SyntaxNodeKind -> ParserFn -> ParserFn
trailingNodeFn kind p c s =
    let initialSize = length (syntax s)    
        new_s = p c s in
    mkTrailingNode new_s kind initialSize

{-
    Like the `node` parser but it also collects
    one syntax that existed before running the given parser.
    This is the left hand side created from the leading parser
    or from previous trailing parsers.
-}
trailingNodeAux :: SyntaxNodeKind -> Parser -> Parser
trailingNodeAux kind p = Parser {
    fn = trailingNodeFn kind (fn p),
    info = nodeInfo kind (info p)
}

{-
    Set the `lhsPrec` in the parser state to the
    given precedence.
-}
setLhsPrecFn :: Int -> ParserFn
setLhsPrecFn precInt _ s =
    if (hasError s) then
        s
    else
        s {lhsPrec = precInt}

setLhsPrec :: Int -> Parser
setLhsPrec precInt = Parser {
    info = ParserInfo {
        collectTokens = \x -> x,
        collectKinds = \x -> x
    },
    fn = setLhsPrecFn precInt
}

checkLhsPrecFn :: Int -> ParserFn
checkLhsPrecFn prec _ s =
    if lhsPrec s >= prec then
        s
    else
        mkError s "unexpected token at this precedence level"

checkLhsPrec :: Int -> Parser
checkLhsPrec prec = Parser {
    info = ParserInfo {
        collectTokens = \x -> x,
        collectKinds = \x -> x
    },
    fn = checkLhsPrecFn prec
}

{-
    Given a precedence `prec` and a parser `p`,
    create a new parser that has the given precedence
    and combines all the resulting syntax made by `p` into a single node.
    This allows it to be used by the pratt parser properly.

    Check that the given precedence `prec` is larger than that in the context.
    Run the parser `node` with `p` as argument.
    Set the lhs precedence of the previously run parser to be `prec` since
    the previously run parser is now `p`.
-}
leadingNode :: SyntaxNodeKind -> Int -> Parser -> Parser
leadingNode n prec p =
    checkPrec prec `andthen` node n p `andthen` setLhsPrec prec 

{-
    Given a precedence `prec`, precedence `lhsPrec`, and parser `p`,
    create a new parser that has the given precedence and `lhsPrec` restriction.

    Check that the given precedence `prec` is larger than that in the context.
    Check that the given `lhsPrec` is smaller than that in the parser state.
    Run the parser `trailingNodeAux` with `p` as argument.
    Set the lhs precedence of the previously run parser to be `prec` since
    the previously run parser is now `p`.
-}
trailingNode :: SyntaxNodeKind -> Int -> Int -> Parser -> Parser
trailingNode kind prec lhsPrec p =
    checkPrec prec `andthen` checkLhsPrec lhsPrec `andthen`
        trailingNodeAux kind p `andthen` setLhsPrec prec

{-
    Maps the name of a token to the list parsers that can parse it.
-}
data TokenMap a = TokenMap (Map String [a])
    deriving Show

{-
    Add a parser to the list of parsers that can parse a given token.
-}
insertTokenMap :: TokenMap a -> String -> a -> TokenMap a
insertTokenMap (TokenMap map) key value =
    TokenMap $
    Map.insertWithKey
        (\key newValue oldValue -> value : oldValue) 
        key [value] map

{-
    Get the list of parsers that can parse a given token.
-}
lookupTokenMap :: TokenMap a -> String -> Maybe [a]
lookupTokenMap (TokenMap map) key = Map.lookup key map

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

restoreState :: ParserState -> Int -> Int -> ParserState
restoreState s initialSize initialPos =
    let lengthDiff = (length (syntax s)) - initialSize in
    s {
        syntax = drop lengthDiff (syntax s),
        pos = initialPos,
        errorMsg = Nothing
    }

{-
    Peek at the next token.

    I'm not certain why we are restoring the state
    instead of just returning the original state.
-}
peekToken :: ParserContext -> ParserState ->
    (ParserState, (Either ParserState Syntax))
peekToken c s =
    let initialSize = length (syntax s)
        iniPos = pos s
        new_s = tokenFn c s in
    if hasError new_s then
        (restoreState new_s initialSize iniPos, Left new_s)
    else
        let stxMaybe = getTopSyntax new_s in
        case stxMaybe of
        Just stx -> (restoreState new_s initialSize iniPos, Right stx)
        Nothing -> (restoreState new_s initialSize iniPos, Left new_s)

{-
    Get the list of all indexed parsers that can parse the next token.
-}
indexed :: forall a. TokenMap a -> ParserContext -> ParserState -> (ParserState, [a])
indexed map c s =
    let (new_s, stx) = peekToken c s
        find :: String -> (ParserState, [a])
        find name =
            case (lookupTokenMap map name) of
            Nothing -> (new_s, [])
            Just as -> (new_s, as)
        in
    case stx of
    Right (Atom sym) -> find sym
    Right (Ident val) -> find identKind
    Right (Node kind _) -> find kind
    Right _ -> (new_s, [])
    Left error_s -> (error_s, [])

{-
    Get all the possibly applicable leading parsers and then
    try them all and use the one with the longest match.
-}
{-leadingParser :: SyntaxNodeKind -> PrattParsingTables -> ParserFn
leadingParser kind tables c s =
    let initialSize = length (syntax s)
        (new_s, indexed_ps) = indexed (leadingTable tables) c s
        in
    if hasError new_s then
        new_s
    else
        let all_ps = (leadingParsers tables) ++ indexed_ps in
        if null all_ps then
            -- If there are no parsers, consume a token and error
            let nextToken_s = tokenFn c new_s in
            if hasError nextToken_s then
                return nextToken_s
            else
                mkUnexpectedTokenError nextToken_s kind
        else
            let parsed_s = longestMatchFn all_ps c new_s in
            mkResult parsed_s initialSize

prattParser :: SyntaxNodeKind -> PrattParsingTables -> ParserFn
prattParser kind tables c s =
    leadingParser kind tables c s-}

commandParser :: ParserFn
commandParser =
    nodeFn "def" (identFn `andthenFn` (symbolFn ":=") `andthenFn` identFn)

{-
runParserCategory :: String -> Syntax
runParserCategory input =
    let p = whitespace `andthenFn` commandParser
    let ctx = mkParserContext input tokens-}

main :: IO ()
main = do
    let c = ParserContext {
        prec = 0,
        inputString = "new := hi test : Type",
        ctxTokens = insert ":=" ":=" (insert "name" "name" empty)
    }
    let s = ParserState {
        syntax = [],
        pos = 0,
        errorMsg = Nothing,
        lhsPrec = 0
    }
    let res = commandParser c s
    putStrLn (show $ syntax res)
    putStrLn (show $ (peekToken c res))
    putStrLn (show $ (peekToken c res))
