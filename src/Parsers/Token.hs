{-
    This file defines the `tokenFn` parser which parses identifiers
    and keywords. It also defines the `whitespace`, `takeUntilFn` and
    `takeWhileFn` parsers.
-}
module Parsers.Token where

import Defs
import Structures
import Data.Char (isAlpha, isAlphaNum)

{-
    Continue incrementing the position in the parser state until
    the predicate is satisfied.
-}
takeUntilFn :: (Char -> Bool) -> ParserFn
takeUntilFn p c s =
    let i = pos s in
    if atEnd c i || p (getInputChar c i) then
        s
    else
        takeUntilFn p c (nextPos s)

{-
    Cotinue incrementing the position in the parser state until
    the predicate is not satisfied.
-}
takeWhileFn :: (Char -> Bool) -> ParserFn
takeWhileFn p = takeUntilFn (not . p)

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
        let stopPos = startPos + length tk
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
        length tk >= (stopPos - startPos)

{-
    Given a token and identifier,
    construct the one that is longer.
-}
mkIdResult :: Int -> Maybe Token -> String -> ParserFn
mkIdResult startPos tk identVal c s =
    let stopPos = pos s in
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
identParseFn startPos tk = parse
    where
    parse :: Name -> ParserFn
    parse r c s =
        let i = pos s in
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
                    stopPos = pos s_new
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