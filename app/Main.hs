module Main (main) where

import Trie
import Data.Char (isAlpha, isAlphaNum, isDigit)
import Data.Map (Map)
import qualified Data.Map as Map

type SyntaxNodeKind = String
type Token = String
type Name = String
type Error = String

numLitKind :: SyntaxNodeKind
numLitKind = "num"

identKind :: SyntaxNodeKind
identKind = "ident"

data Syntax = Missing
    | Node SyntaxNodeKind [Syntax]
    | Atom String
    | Ident String
    deriving Show

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

data Parser = Parser {
    info :: ParserInfo,
    fn :: ParserFn
}

data ParserInfo = ParserInfo {
    -- Adds the tokens relevant for the parser
    collectTokens :: [Token] -> [Token],
    -- Adds the kinds relevant for the parser
    collectKinds :: [SyntaxNodeKind] -> [SyntaxNodeKind]
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
pushSyntax s n = s {syntax = n : (syntax s)}

popSyntax :: ParserState -> ParserState
popSyntax s =
    case syntax s of
    [] -> s
    x : xs -> s {syntax = xs}

setError :: ParserState -> Error -> ParserState
setError s e = s { errorMsg = Just e }

{-
    Sets the error message and pushes `Missing`
    onto the syntax list.
-}
mkError :: ParserState -> Error -> ParserState
mkError s e = pushSyntax (setError s e) Missing

mkUnexpectedTokenError :: ParserState -> Error -> ParserState
mkUnexpectedTokenError s e = mkError (popSyntax s) e

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
    `inputString` is the entire string we are parsing
    `tokens` stores the possible tokens we can parse
-}
data ParserContext = ParserContext {
    prec :: Int,
    inputString :: String,
    ctxTokens :: TokenTable
}

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
getInputChar c p = (inputString c) !! p


{-
    Increment the current position in the parser state
-}
nextPos :: ParserState -> ParserState
nextPos s = s { pos = (pos s) + 1}

type ParserFn = ParserContext -> ParserState -> ParserState

{-
    Continue incrementing the position in the parser state until
    the predicate is satisfied.
-}
takeUntilFn :: (Char -> Bool) -> ParserFn
takeUntilFn p c s =
    let i = (pos s) in
    if atEnd c i || p (getInputChar c i) then
        s
    else
        takeUntilFn p c (nextPos s)

{-
    Cotinue incrementing the position in the parser state until
    the predicate is not satisfied.
-}
takeWhileFn :: (Char -> Bool) -> ParserFn
takeWhileFn p = takeUntilFn (\c -> not (p c))

{-
    If a token is given, then construct the atom syntax for it
    and update the current position.
    Otherwise, error at the start position.
-}
mkTokenAndFixPos :: Int -> Maybe Token -> ParserFn
mkTokenAndFixPos startPos tk c s =
    case tk of
    Nothing -> mkErrorAt s "token" startPos
    Just tk ->
        let stopPos = startPos + (length tk)
            sNew = s {pos = stopPos} 
            sNewWhitespace = whitespace c sNew in 
        pushSyntax sNewWhitespace (Atom tk)

{-
    Consider many unicode characters as being valid letters
    (Exludes things like +, ×, λ) 
-}
isLetterLike :: Char -> Bool
isLetterLike c =
  let v = fromEnum c
  in (0x3b1  <= v && v <= 0x3c9 && v /= 0x3bb) ||
     (0x391  <= v && v <= 0x3A9 && v /= 0x3A0 && v /= 0x3A3) ||
     (0x3ca  <= v && v <= 0x3fb) ||
     (0x1f00 <= v && v <= 0x1ffe) ||
     (0x2100 <= v && v <= 0x214f) ||
     (0x1d49c <= v && v <= 0x1d59f) ||
     (0x00c0 <= v && v <= 0x00ff && v /= 0x00d7 && v /= 0x00f7) ||
     (0x0100 <= v && v <= 0x017f)

{-
    Is true for subscripts that are numbers
-}
isNumericSubscript :: Char -> Bool
isNumericSubscript c =
  let v = fromEnum c
  in 0x2080 <= v && v <= 0x2089

{-
    Is true for many alphanumeric unicode subscripts
-}
isSubScriptAlnum :: Char -> Bool
isSubScriptAlnum c =
  let v = fromEnum c
  in isNumericSubscript c ||
     (0x2090 <= v && v <= 0x209c) ||
     (0x1d62 <= v && v <= 0x1d6a) ||
     v == 0x2c7c

{-
    Says which characters can start an identifier
-}
isIdFirst :: Char -> Bool
isIdFirst c = isAlpha c || c == '_' || isLetterLike c

{-
    Says which characters can appear after the first letter
    of an identifier.
-}
isIdRest :: Char -> Bool
isIdRest c = isAlphaNum c || c == '_' || c == '\'' || c == '!' ||
    c == '?' || isLetterLike c || isSubScriptAlnum c

{-
    Extract the sublist in the range [start, stop)
-}
extractSublist :: [a] -> Int -> Int -> [a]
extractSublist str start stop =
    take (stop - start) (drop start str)

{-
    Given the start and stop position of an identifier
    and a possible token,
    return true if the token is longer than the identifier.
-}
isToken :: Int -> Int -> Maybe Token -> Bool
isToken startPos stopPos tk =
    case tk of
    Nothing -> False
    Just tk ->
        (length tk) >= (stopPos - startPos)

{-
    Given a token and identifier,
    construct the one that is longer.
-}
mkIdResult :: Int -> Maybe Token -> String -> ParserFn
mkIdResult startPos tk identVal c s =
    let stopPos = (pos s) in
    -- If the token is longer than the identifier, make a token
    if isToken startPos stopPos tk then
        mkTokenAndFixPos startPos tk c s
    -- Otherwise, construct the identifier
    else
        let new_s = whitespace c s in
        pushSyntax new_s (Ident identVal)

{-
    Parse identifiers and tokens.
    `tk` is the longest possible token it could be.

    If the current position can be parsed as an identifier,
    then the result is the longer match between
    the identifier and possible token.

    If it cannot be parsed as an identifier, then the result is the token.
    Or an error if there if no possible token was provided.
-}
identParseFn :: Int -> Maybe Token -> Name -> ParserFn
identParseFn startPos tk r = parse r
    where
    parse :: Name -> ParserFn
    parse r c s =
        let i = (pos s) in
        -- If we begin parsing at the end, then error
        if atEnd c i then
            mkEOIError s
        else
            let curr = getInputChar c i in
            -- Attempt to parse the input as an identifier
            if isIdFirst curr then
                let startPos = i
                    -- Continue consuming characters that are valid
                    s_new = takeWhileFn isIdRest c (nextPos s)
                    stopPos = (pos s_new)
                    -- Extract the substring representing the identifier
                    identVal = extractSublist (inputString c) startPos stopPos
                in
                -- Either construct this identifier or use the token
                mkIdResult startPos tk identVal c s_new
            -- Otherwise it can only be a token
            else
                mkTokenAndFixPos startPos tk c s

{-
    Add the next identifier or token to the
    top of the syntax list in the parser state.
-}
tokenFn :: ParserFn
tokenFn c s =
    let i = pos s
        curr = getInputChar c i
        tk = matchPrefix (inputString c) (ctxTokens c) i in
    identParseFn i tk "anonymous" c s

{-
    Is true for space, tab, carriage return, or newline
-}
isWhitespace :: Char -> Bool
isWhitespace c = c == ' ' || c == '\t' || c == '\r' || c == '\n'

{-
    Increments the current position so long as
    the character is whitespace
-}
whitespace :: ParserFn
whitespace c s =
    let i = pos s in
    if atEnd c i then
        s
    else
        let curr = getInputChar c i in
        if curr == '\t' then
            mkError s "tabs not allowed"
        else if curr == '\r' then
            mkError s "isolated carriate return not allowed"
        else if isWhitespace curr then
            whitespace c (nextPos s)
        else
            s

getTopSyntax :: ParserState -> Maybe Syntax
getTopSyntax s =
    case (syntax s) of
    [] -> Nothing
    x : xs -> Just x

{-
    Parse the next token and check that it
    satisfies the predicate. Otherwise error. 
-}
satisfySymbolFn :: (String -> Bool) -> ParserFn
satisfySymbolFn p c s =
    let iniPos = pos s
        s_new = tokenFn c s
    in
    if hasError s_new then
        s_new
    else
        case syntax s_new of
        [] -> mkError s_new "no syntax in parser state"
        (Atom sym) : _ ->
            if p sym then
                s_new
            else
                mkUnexpectedTokenError s_new ("unexpected token: " ++ sym)
        _ -> mkUnexpectedTokenError s_new "unexpected identifier"

{-
    Parses the next part of the input
    and checks that it is a token equal to the given string.
-}
symbolFn :: String -> ParserFn
symbolFn sym = satisfySymbolFn (\s -> s == sym)

{-
    Adds the given string to the list of tokens.
-}
symbolInfo :: String -> ParserInfo
symbolInfo sym = ParserInfo {
    collectTokens = \tks -> sym : tks,
    collectKinds = \kinds -> kinds
}

{-
    Given a string, construct a parser that parses
    this string as a token.
    `info` stores this string as a new token

    `fn` expects that the next part of the input is the
    given string and parses it, errors otherwise
-}
symbol :: String -> Parser
symbol sym = Parser {
    info = symbolInfo sym,
    fn = symbolFn sym
}

{-
    Run the first parser and then the second
-}
andthenFn :: ParserFn -> ParserFn -> ParserFn
andthenFn p q c s =
    let s_new = p c s in
    if hasError s then
        s
    else
        q c s_new

{-
    Combine the collected tokens and kinds of the two parsers
-}
andthenInfo :: ParserInfo -> ParserInfo -> ParserInfo
andthenInfo p q = ParserInfo {
    collectTokens = (collectTokens p) . (collectTokens q),
    collectKinds = (collectKinds p) . (collectKinds q)
}

andthen :: Parser -> Parser -> Parser
andthen p q = Parser {
    info = andthenInfo (info p) (info q),
    fn = andthenFn (fn p) (fn q)
}

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
    Check if the syntax has the given kind
-}
isOfKind :: Syntax -> SyntaxNodeKind -> Bool
isOfKind stx k = (getKind stx) == k

{-
    Consumes a token and checks that it has the
    expected syntax kind. Otherwise, error.
-}
expectTokenFn :: SyntaxNodeKind -> String -> ParserFn
expectTokenFn k desc c s =
    let new_s = tokenFn c s in
    -- If there isn't already an error,
    -- then check if we have the expected kind
    if not (hasError new_s) then
        case getTopSyntax new_s of
        Nothing -> mkError new_s "no syntax"
        Just stx ->
            if not (isOfKind stx k) then
                mkUnexpectedTokenError new_s desc
            else
                new_s
    else
        new_s

{-
    Consumes a single identifier.
    If the next token is not an identifier, error.
-}
identFn :: ParserFn
identFn = expectTokenFn identKind "identifier"

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
        inputString = "new := hi : Type",
        ctxTokens = insert ":=" ":=" (insert "name" "name" empty)
    }
    let s = ParserState {
        syntax = [],
        pos = 0,
        errorMsg = Nothing
    }
    let res = commandParser c s
    putStrLn (show $ syntax res)
