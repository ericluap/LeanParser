{-
    Defines the parsing function `categoryParser` and
    functions for adding new syntax to categories.
-}
module Category where

import Defs
import Structures
import Pratt.Basic
import qualified Data.Map as Map
import Data.Set (Set)

{-
    Parses the given category.

    For example, `categoryParser "command"` parses a single command.
-}
categoryParser :: String -> ParserFn
categoryParser catName c s =
    case Map.lookup catName (categories c) of
    Nothing -> mkError s ("unknown parser category " ++ catName)
    Just tables -> prattParser catName tables c s

{-
    Add a single token to the `ctxTokens` in the parser context.

    NOTE: Why bother checking if it exists already? It seems to me like
    you should just `insert`.
-}
addToken :: Token -> ParserContext -> ParserContext
addToken token ctx =
    case maybeFind token (ctxTokens ctx) of
    Nothing ->
        let newCtxToken = insert token token (ctxTokens ctx) in
        ctx {ctxTokens = newCtxToken}
    Just _ -> ctx

{-
    Add all the tokens that the parser uses
    to the collection of tokens in the parser context.
-}
addParserTokens :: Parser -> ParserContext -> ParserContext
addParserTokens p ctx =
    let tokens = collectTokens (info p) [] in
    foldr addToken ctx tokens 

{-
    Add all used parser tokens to `ctxTokens` and then
    add the parser to the Pratt parsing tables for the given category
    either as an indexed or non-indexed leading parser
    depending on the parser's `firstTokens`.
-}
addLeadingParser :: String -> Parser -> ParserContext -> Either String ParserContext
addLeadingParser catName p ctx =
    case Map.lookup catName (categories ctx) of
    Nothing -> Left ("unknown parser category " ++ catName)
    Just tables ->
        let tokens_ctx = addParserTokens p ctx
            -- Adds the parser as an indexed parser
            -- using each of its possible first tokens as an index.
            addIndexed :: Set Token -> ParserContext
            addIndexed tks =
                let newTables = foldr
                        (\tk currTables -> currTables {leadingTable =
                            insertTokenMap (leadingTable currTables) tk p})
                        tables tks
                    in
                tokens_ctx {categories =
                    Map.insert catName newTables (categories tokens_ctx)}
            in
        case firstTokens (info p) of
        Tokens tks -> Right (addIndexed tks)
        OptTokens tks -> Right (addIndexed tks)
        _ ->
            -- Add the parser as a non-indexed parser
            let newTables = tables {leadingParsers = p : leadingParsers tables} in
            Right tokens_ctx {categories =
                Map.insert catName newTables (categories tokens_ctx)}