module Language (main) where

import LeanParser

argPrec :: Int
argPrec = 1023

leadPrec :: Int
leadPrec = 1022

minPrec :: Int
minPrec = 10


{---- Term parsers ----}
term :: String
term = "term"

{-- Nat parsers --}
nat :: Parser
nat = leadingNode "nat" maxPrec (symbol "Nat")

succNat :: Parser
succNat = leadingNode "succ" maxPrec (symbol "S")

recNat :: Parser
recNat = leadingNode "recNat" maxPrec (symbol "recNat")

{-- List parsers --}
list :: Parser
list = leadingNode "list" maxPrec (symbol "List")

bracketList :: Parser
bracketList = leadingNode "bracketList" maxPrec
    (symbol "[" `andthen` sepBy (categoryParser term 0) (symbol ",")
    `andthen` symbol "]")

listCons :: Parser
listCons = trailingNode "cons" 67 68
    (symbol "::" `andthen` categoryParser term 67)

{-- Stream parsers --}
stream :: Parser
stream = leadingNode "stream" maxPrec (symbol "Stream")

fby :: Parser
fby = trailingNode "fby" 65 66 (symbol "fby" `andthen` categoryParser term 66)

{-- Later parsers --}
later :: Parser
later = leadingNode "later" 100 (symbol "▹" `andthen` categoryParser term 100)

next :: Parser
next = leadingNode "next" 80 (symbol "next" `andthen` categoryParser term 80)

paren :: Parser
paren = leadingNode "paren" maxPrec (withoutPosition
    (symbol "(" `andthen` categoryParser term 0 `andthen` symbol ")"))

arrow :: Parser
arrow = trailingNode "arrow" 25 0
    (symbol "→" `andthen` categoryParser term 25)

funBinder :: Parser
funBinder = ident `andthen` symbol ":" `andthen` categoryParser term 0

fun :: Parser
fun = leadingNode "fun" maxPrec
    (symbol "fun" `andthen` funBinder `andthen` symbol "." `andthen`
    categoryParser term 0)

{-
    `checkColGt` is needed here as otherwise it will attempt to
    parse the identifier that starts the next command as an argument
    to the end of the current term
-}
argument :: Parser
argument = checkColGt `andthen` categoryParser term argPrec

{-
    Since the lhs is `maxPrec`, only `maxPrec` things can be applied.
    This means that application is not left associative as the result of an
    application is not high enough precedence to be applied.

    Since the rhs is `argPrec`, only things at least `arcPrec` can be arguments.
    This means that application is not right associative as application is
    not high enough precedence to be considered as an argument.

    If something is `argPrec` (and so less than `maxPrec`),
    it can be an argument but cannot be applied.
-}
app :: Parser
app = trailingNode "app" leadPrec maxPrec (many1 argument)

addTermParsers :: ParserContext -> ParserContext
addTermParsers rules =
    let trailingRules = addTrailingParsers term [arrow, app, listCons, fby]
            rules
        allRules = addLeadingParsers term
            [ident, num, nat, paren, fun, recNat, succNat, bracketList, stream,
            later]
            trailingRules in
    allRules

{---- Command parsers ----}
command :: String
command = "command"

typeSpec :: Parser
typeSpec = leadingNode "typeSpec" maxPrec (withPosition
    (ident `andthen` symbol ":" `andthen` categoryParser term 0))

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
    input <- readFile "examples/LanguageInput.txt"
    let (maybeError, syntax) = parse input allRules
    putStrLn (concatMap withParentheses syntax)
    putStrLn ("Errors: " ++ show maybeError)