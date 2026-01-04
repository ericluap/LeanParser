module Language (main) where

import LeanParser

argPrec :: Int
argPrec = 1023

leadPrec :: Int
leadPrec = 1022

minPrec :: Int
minPrec = 10


{- Term parsers -}
term :: String
term = "term"

nat :: Parser
nat = leadingNode "nat" maxPrec (symbol "Nat")

list :: Parser
list = leadingNode "list" maxPrec (symbol "List")

paren :: Parser
paren = leadingNode "paren" maxPrec
    (symbol "(" `andthen` categoryParser term 0 `andthen` symbol ")")

arrow :: Parser
arrow = trailingNode "arrow" 25 0
    (symbol "→" `andthen` categoryParser term 25)

funBinder :: Parser
funBinder = ident `andthen` symbol ":" `andthen` categoryParser term 0

fun :: Parser
fun = leadingNode "fun" maxPrec
    (symbol "fun" `andthen` funBinder `andthen` symbol "." `andthen`
    categoryParser term 0)

argument :: Parser
argument = checkColGt `andthen` categoryParser term argPrec

{-
    Since `lhsPrec` is `maxPrec`, the lhs cannot be an argument or
    an application. Since the rhs is `argPrec`, it cannot be an application.
-}
app :: Parser
app = trailingNode "app" leadPrec maxPrec (many1 argument)

addTermParsers :: ParserContext -> ParserContext
addTermParsers rules =
    let trailingRules = addTrailingParsers term [arrow, app] rules
        allRules = addLeadingParsers term
            [ident, num, nat, paren, fun] trailingRules in
    allRules

{- Command parsers -}
command :: String
command = "command"

typeSpec :: Parser
typeSpec = leadingNode "typeSpec" maxPrec (withPosition
    (ident `andthen` symbol "::" `andthen` categoryParser term 0))

definition :: Parser
definition = leadingNode "definition" maxPrec (withPosition
    (ident `andthen` symbol ":=" `andthen` categoryParser term 0))

addCommandParsers :: ParserContext -> ParserContext
addCommandParsers rules =
    let trailingRules = addTrailingParsers command [] rules
        allRules = addLeadingParsers command
            [typeSpec, definition] trailingRules in
    allRules

main :: IO ()
main = do
    let termRules = addTermParsers emptyParsingRules
    let allRules = addCommandParsers termRules
    let (maybeError, syntax) = parse "\n \
                    \test :: Nat → Nat → Nat\n\
                    \test := fun x : Nat . x 2 y" allRules
    putStrLn (concatMap withParentheses syntax)
    putStrLn ("Errors: " ++ show maybeError)