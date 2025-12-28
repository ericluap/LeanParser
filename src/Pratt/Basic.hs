{-# LANGUAGE ScopedTypeVariables #-}

{-
    This file defines the `prattParser` combinator and the tables
    it uses to store information.
-}

module Pratt.Basic where

import Defs
import Data.Map (Map)
import Structures
import Parsers.Basic

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

{-
    Stores all the parsing information for a category.
-}
data ParserCategory = ParserCategory {
    tables :: PrattParsingTables
}

{-
    Stores all the parsing rules set up to parse the language.

    `categories` maps the name of a category to all the parsing rules
    stored for that category.
-}
data State = State {
    categories :: Map String ParserCategory
}

{-
    Get the list of all indexed parsers that can parse the next token.
-}
indexed :: forall a. TokenMap a -> ParserContext -> ParserState -> (ParserState, [a])
indexed map c s =
    let (new_s, stx) = peekToken c s
        -- Find the relevant parsers given the name of the next token
        find :: String -> (ParserState, [a])
        find name =
            case (lookupTokenMap map name) of
            Nothing -> (new_s, [])
            Just as -> (new_s, as)
        in
    -- Extract the name of the next token and pass it to `find`
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