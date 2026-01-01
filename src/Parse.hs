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
    let result = (whitespace `andthenFn` categoryParser "command") c s in
    if hasError result || atEnd c (pos result) then
        result
    else
        parseAllCommands c result

{-
    Given the input string and a parser context storing the parsing rules,
    parse all the commands in the file.
-}
parse :: String -> ParserContext -> Either String [Syntax]
parse input ruleCtx =
    let initialCtx = ruleCtx {
            prec = 0,
            inputString = input
        } 
        initialState = ParserState {
            syntax = [],
            pos = 0,
            errorMsg = Nothing,
            lhsPrec = 0
        }
        result = parseAllCommands initialCtx initialState 
        in
    case errorMsg result of
    Just error -> Left error
    Nothing -> Right $ reverse $ map reverseSyntax (syntax result)