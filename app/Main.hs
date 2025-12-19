module Main (main) where

import Trie
import Data.Char (isAlpha, isAlphaNum)
import Data.Map (Map)
import qualified Data.Map as Map

type SyntaxNodeKind = String
type Token = String
type Name = String
type Error = String

data Syntax = Missing
    | Node SyntaxNodeKind [Syntax]
    | Atom String
    | Ident String
    deriving Show
{-
    `pos` is the current position of the character we are parsing
    within the input string
-}
data ParserState = ParserState {
    syntax :: [Syntax],
    pos :: Int,
    errorMsg :: Maybe Error
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

mkUnexpectedError :: ParserState -> Error -> ParserState
mkUnexpectedError s e = mkError (popSyntax s) e

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
            sNew = s {pos = stopPos} in 
        pushSyntax sNew (Atom tk)

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
    Extract the substring in the range [start, stop)
-}
extractSubstring :: String -> Int -> Int -> String
extractSubstring str start stop =
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
        pushSyntax s (Ident identVal)

{-
    Parse identifiers and tokens.
    `tk` is the longest possible token it could be.

    If the current position can be parsed as an identifier,
    then the result is the longer match between
    the identifier and possible token.

    If it cannot be parsed as an identifier, then the result is the token.
    Or an error if there if no possible token was provided.
-}
identFn :: Int -> Maybe Token -> Name -> ParserFn
identFn startPos tk r = parse r
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
                    identVal = extractSubstring (inputString c) startPos stopPos
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
    identFn i tk "anonymous" c s

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
                mkUnexpectedError s_new "unexpected token"
        _ -> mkUnexpectedError s_new "unexpected identifier"

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

main :: IO ()
main = do
    let c = ParserContext {
        prec = 0,
        inputString = "name hi : Type",
        ctxTokens = insert "hi " "hi " (insert "name " "name " empty)
    }
    let s = ParserState {
        syntax = [],
        pos = 0,
        errorMsg = Nothing
    }
    let testsym = symbol "name " `andthen` symbol "hi "
    let res = (fn testsym) c s
    putStrLn (show $ syntax res)

    let trie1 = empty
    let trie2 = (insert "def" 5 trie1)
    let trie3 = (insert "definition" 6 trie2)
    let match = matchPrefix "definitions" trie3 0
    putStrLn (show match)
