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
import Parsers.Token

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
    Runs the parser and checks that only one
    piece of syntax is created.

    This is used by both leading and trailing parsers
    and so does some case work.

    `left?` is `Nothing` for leading parsers.
    For trailing parsers, it is the syntax of the left hand side.

    `startLhsPrec` is the lhsPrec of `left?`
    (the starting lhsPrec as opposed to the lhsPrec of the parser
    that was previously attempted in the process of finding the longest match).
    If `left?` is `Nothing`, then this is a leading parser so it doesn't
    matter what lhsPrec is (we set it to `maxPrec` in case the parser that runs
    does not set `lhsPrec`).

    The lhsPrec is set manually here because the previously tried
    parser may have changed it. Even though we restore the state after
    running the previous parser, the `restoreState` function does
    not restore the lhsPrec for some reason.
-}
runLongestMatchParser :: Maybe Syntax -> Int -> ParserFn -> ParserFn
runLongestMatchParser maybeLeft startLhsPrec p c s =
    let startSize = length (syntax s)
        start_s = case maybeLeft of
            Just left -> pushSyntax (s {lhsPrec = startLhsPrec}) left
            Nothing -> s {lhsPrec = maxPrec}
        new_s = p c start_s
        in
    -- success or error, it at least has the right number of nodes
    if length (syntax new_s) == startSize + 1 then
        new_s 
    -- if it has the wrong number of nodes and an error,
    -- replace the added nodes with Missing
    else if hasError new_s then
        let lengthDiff = length (syntax new_s) - startSize
            newSyntax = drop lengthDiff (syntax new_s)
            in
        pushSyntax (new_s {syntax = newSyntax}) Missing
    -- the parser suceeded with the wrong number of nodes,
    -- so this is not a properly set up parser
    else
        mkError new_s "longestMatch parsers must generate exactly one Syntax node"

{-
    Run the given parser and check if it has a longer match than
    the previously attempted parser. If so, keep the resuling state.
    Otherwise, keep the state of the previously attempted one.

    NOTE: This function differs from Lean's.
    Lean doesn't just compare the length of the match, but also whether or not
    there was an error and the given priority of each parser. If still the
    results are equal, it creates a choice node (a node that stores
    the results of both parses).
-}
longestMatchStep :: Maybe Syntax -> Int -> Int -> Int -> ParserFn -> ParserFn
longestMatchStep maybeLeft startSize startLhsPrec startPos p c s =
    let starting_s = restoreState s startSize startPos
        end_s = runLongestMatchParser maybeLeft startLhsPrec p c starting_s
        in
    if pos end_s > pos s then
        end_s
    else
        s

{-
    Given a list of parsers, try running each of them and return the resulting
    state of the parser with the longest match.
-}
longestMatchFnAux :: Maybe Syntax -> Int -> Int -> Int -> [Parser] -> ParserFn
longestMatchFnAux maybeLeft startSize startLhsPrec startPos ps =
    let parse :: [Parser] -> ParserFn
        parse ps =
            case ps of
            [] -> (\_ s -> s)
            p : ps -> (\c s ->
                let new_s = longestMatchStep maybeLeft startSize startLhsPrec
                        startPos (fn p) c s 
                    in
                parse ps c new_s)
        in
    parse ps

{-
    Given a list of parsers, try running each of them and return the resulting
    state of the parser with the longest match.

    NOTE: Since Lean uses priority as well, it runs the first parser manually
    in order to get its priority and then calls `longestMatchFnAux`.
-}
longestMatchFn :: Maybe Syntax -> [Parser] -> ParserFn
longestMatchFn maybeLeft ps c s =
    let startSize = length (syntax s)
        startLhsPrec = lhsPrec s
        startPos = pos s
        in
    longestMatchFnAux maybeLeft startSize startLhsPrec startPos ps c s

{-
    Get all the possibly applicable leading parsers and then
    try them all and use the one with the longest match.

    NOTE: Lean wraps the result in a choice node if more than one piece
    of syntax is produced. Since our longest match function just
    arbitrarily chooses a longest match, we do not have choice nodes.
-}
leadingParser :: SyntaxNodeKind -> PrattParsingTables -> ParserFn
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
                nextToken_s
            else
                mkUnexpectedTokenError nextToken_s kind
        else
            longestMatchFn Nothing all_ps c new_s

{-
    Get all the possibly applicable trailing parsers and then
    try them all and use the one with the longest match.
    Continue doing this until no trailing parsers succeed.
-}
trailingLoop :: PrattParsingTables -> ParserFn
trailingLoop tables c s =
    let initialSize = length (syntax s)
        initialPos = pos s
        (new_s, indexed_ps) = indexed (trailingTable tables) c s
        in
    if hasError new_s then
        -- Discard the error and let the next leading parser have the error
        restoreState s initialSize initialPos
    else
        let all_ps = indexed_ps ++ (trailingParsers tables) in
        if null all_ps then
            new_s
        else
            let maybeLeft = getTopSyntax new_s
                noLeft_s = popSyntax new_s
                afterStep_s = longestMatchFn maybeLeft all_ps c noLeft_s
                in
            if hasError afterStep_s then
                -- If the longest matching parser fails but does not consume
                -- any tokens, then discard the error.
                if pos afterStep_s == initialPos then
                    case maybeLeft of
                    Nothing -> restoreState afterStep_s initialSize initialPos
                    Just left -> pushSyntax 
                        (restoreState afterStep_s (initialSize - 1) initialPos) left
                else
                    afterStep_s
            else
                trailingLoop tables c afterStep_s

{-
    Parse according to the parsers in the tables.
    
    First parse using the longest matching leading parser.
    Then repeatedly parse using the longest matching trailing parsers.

    Nothing in this function checks or handles precedence.
    Instead, the leading and trailing parsers check and set the
    precedences themselves
    (`leadingNode` and `trailingNode` call `checkPrec` and `checkLhsPrec`).
-}
prattParser :: SyntaxNodeKind -> PrattParsingTables -> ParserFn
prattParser kind tables c s =
    let new_s = leadingParser kind tables c s in
    if hasError new_s then
        new_s
    else
        trailingLoop tables c new_s