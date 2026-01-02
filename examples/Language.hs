module Language (main) where

import LeanParser

{- Term parsers -}
term :: String
term = "term"

arrow :: Parser
arrow = trailingNode "arrow" 25 0
    (symbol "->" `andthen` categoryParser term 25)

addTermParsers :: ParserContext -> ParserContext
addTermParsers rules =
    let trailingRules = addTrailingParsers term [arrow] rules
        allRules = addLeadingParsers term [ident] trailingRules in
    allRules

{- Command parsers -}
command :: String
command = "command"

typeSpec :: Parser
typeSpec = leadingNode "typeSpec" maxPrec
    (ident `andthen` symbol "::" `andthen` categoryParser term 0)

definition :: Parser
definition = leadingNode "definition" maxPrec
    (ident `andthen` symbol ":=" `andthen` categoryParser term 0)

addCommandParsers :: ParserContext -> ParserContext
addCommandParsers rules =
    let trailingRules = addTrailingParsers command [] rules
        allRules = addLeadingParsers command
            [typeSpec, definition] trailingRules in
    allRules

main :: IO ()
main = do
    let termRules = addTermParsers emptyParsingRules
        allRules = addCommandParsers termRules
        res = parse "\n \
                    \test :: Nat -> Nat -> Nat \n\
                    \test := something \n\
                    \" allRules
    case res of
        Left error -> print error
        Right stx -> putStrLn (concatMap withParentheses stx)