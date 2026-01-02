module Language (main) where

import LeanParser

{- Term parsers -}
{-term :: String
term = "term"

arrow :: Parser
arrow = trailingNode "arrow" 25 0
    (symbol "->" `andthen` categoryParser term 25)

addTermParsers :: ParserContext -> ParserContext
addTermParsers rules =
    let trailingRules = addTrailingParsers term [arrow] rules
        allRules = addLeadingParsers term [] trailingRules in
    allRules

{- Command parsers -}
command :: String
command = "command"

typeSpec :: Parser
typeSpec = leadingNode "typeSpec" maxPrec
    (ident `andthen` symbol "::" `andthen` categoryParser term 0)

definition :: Parser
definition = leadingNode "definition" maxPrec
    (optional typeSpec `andthen` checkLinebreakBefore)-}

main :: IO ()
main = do
    print "hi"