{-
    This file defines the `parse` function which takes the input
    string and a `ParserContext` with rules and produces the result
    of parsing all the commands.
-}
module Parse where

import Defs
import Category (categoryParser)
import Parsers.Basic (andthenFn)
import Parsers.Token (whitespace)
import Position (fileMapOfString)

{-
    Reverse all the children of any `Node` appearing in the syntax.
-}
reverseSyntax :: Syntax -> Syntax
reverseSyntax (Node kind children) =
    Node kind (reverse $ map reverseSyntax children)
reverseSyntax other = other

{-
    Parse all the commands.
    If there is an error, stop parsing.
-}
parseAllCommands :: ParserFn
parseAllCommands c s =
    let startPos = pos s
        result = (whitespace `andthenFn` fn (categoryParser "command" 0)) c s in
    if hasError result || atEnd c (pos result) then
        result
    else if startPos == pos result then
        mkError result "no progress"
    else
        parseAllCommands c result

updateCtxWithInputString :: String -> ParserContext -> ParserContext
updateCtxWithInputString input ctx = ctx {
        prec = 0,
        inputString = input,
        fileMap = fileMapOfString input,
        savedPos = Nothing
    } 

runParserCategory :: String -> String -> ParserContext -> Either String [Syntax]
runParserCategory input catName ruleCtx =
    let initialCtx = updateCtxWithInputString input ruleCtx
        initialState = ParserState {
            syntax = [],
            pos = 0,
            errorMsg = Nothing,
            lhsPrec = 0
        }
        result = (whitespace `andthenFn` fn (categoryParser catName 0))
            initialCtx initialState 
        in
    case errorMsg result of
    Just error -> Left error
    Nothing -> Right $ reverse $ map reverseSyntax (syntax result)

{-
    Given the input string and a parser context storing the parsing rules,
    parse all the commands in the file.
-}
parse :: String -> ParserContext -> (Maybe Error, [Syntax])
parse input ruleCtx =
    let initialCtx = updateCtxWithInputString input ruleCtx
        initialState = ParserState {
            syntax = [],
            pos = 0,
            errorMsg = Nothing,
            lhsPrec = 0
        }
        result = parseAllCommands initialCtx initialState
        reversedSyntax = reverse $ map reverseSyntax (syntax result)
        in
    (errorMsg result, reversedSyntax)