{-
    This file defines basic parsers like
    `symbol`, `andthen`, and `expectTokenFn`.
-}
module Parsers.Basic where

import Defs
import qualified Data.Set as Set
import Parsers.Token

{-
    Parse the next token and check that it
    satisfies the predicate. Otherwise error. 
-}
satisfySymbolFn :: (String -> Bool) -> ParserFn
satisfySymbolFn p c s =
    let s_new = tokenFn c s in
    if hasError s_new then
        s_new
    else
        case syntax s_new of
        [] -> mkError s_new "no syntax in parser state"
        (Atom _ sym) : _ ->
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
symbolFn sym = satisfySymbolFn (== sym)

{-
    Adds the given string to the list of tokens.
-}
symbolInfo :: String -> ParserInfo
symbolInfo sym = ParserInfo {
    collectTokens = (sym :),
    firstTokens = Tokens (Set.singleton sym)
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
    if hasError s_new then
        s_new
    else
        q c s_new

{-
    Combine the collected tokens and kinds of the two parsers
-}
andthenInfo :: ParserInfo -> ParserInfo -> ParserInfo
andthenInfo p q = ParserInfo {
    collectTokens = collectTokens p . collectTokens q,
    firstTokens = seqFirstTokens (firstTokens p) (firstTokens q)
}

andthen :: Parser -> Parser -> Parser
andthen p q = Parser {
    info = andthenInfo (info p) (info q),
    fn = andthenFn (fn p) (fn q)
}

{-
    Check if the syntax has the given kind
-}
isOfKind :: Syntax -> SyntaxNodeKind -> Bool
isOfKind stx k = getKind stx == k

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

ident :: Parser
ident = Parser {
    fn = identFn,
    info = ParserInfo {
        collectTokens = id,
        firstTokens = Tokens (Set.singleton identKind)
    }
}

numFn :: ParserFn
numFn = expectTokenFn numLitKind "numeral"

num :: Parser
num = Parser {
    fn = numFn,
    info = ParserInfo {
        collectTokens = id,
        firstTokens = Tokens (Set.singleton numLitKind)
    }
}

restoreState :: ParserState -> Int -> Int -> ParserState
restoreState s initialSize initialPos =
    let lengthDiff = length (syntax s) - initialSize in
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
    (ParserState, Either ParserState Syntax)
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
        Nothing ->
            let error_s = mkError new_s "missing top syntax" in
            (restoreState new_s initialSize iniPos, Left error_s)

{-
    Check that the trailing whitespace in the most
    recently parsed `SourceInfo` has a line break.
-}
checkTailLinebreak :: Syntax -> Bool
checkTailLinebreak stx =
    case getTailInfoMaybe stx of
    Nothing -> False
    Just tailInfo -> '\n' `elem` trailing tailInfo

{-
    Checks that the most recently parsed syntax has a trailing
    line break. Errors otherwise.
-}
checkLinebreakBeforeFn :: ParserFn
checkLinebreakBeforeFn _ s =
    case getTopSyntax s of
    Nothing -> mkError s "line break"
    Just stx ->
        if checkTailLinebreak stx then
            s
        else
            mkError s "line break"

{-
    Checks that the most recently parsed syntax has a trailing
    line break. Errors otherwise.
-}
checkLinebreakBefore :: Parser
checkLinebreakBefore = Parser {
    fn = checkLinebreakBeforeFn,
    info = ParserInfo {
        collectTokens = id,
        firstTokens = Epsilon
    }
}

{-
    If the given parser fails without consuming input,
    then skip over it.
-}
optionalFn :: ParserFn -> ParserFn
optionalFn p c s =
    let initialSize = length (syntax s)
        initialPos = pos s
        new_s = p c s in
    if hasError new_s && pos new_s == initialPos then
        restoreState new_s initialSize initialPos
    else
        new_s

optionalInfo :: ParserInfo -> ParserInfo
optionalInfo p = ParserInfo {
    collectTokens = collectTokens p,
    firstTokens = toOptional (firstTokens p)
}

{-
    If the given parser fails without consuming input,
    then skip over it.
-}
optional :: Parser -> Parser
optional p = Parser {
    fn = optionalFn (fn p),
    info = optionalInfo (info p)
}