module Arithmetic (main) where

import LeanParser

command :: String
command = "command"

plus :: Parser
plus = trailingNode "plus" 65 65
    (symbol "+" `andthen` categoryParser command 66)

mul :: Parser
mul = trailingNode "mul" 70 70
    (symbol "*" `andthen` categoryParser command 71)

main :: IO ()
main = do
    let leadingCtx = addLeadingParser command num emptyParsingRules
    let rulesCtx = addTrailingParsers command [plus, mul] leadingCtx
    let (maybeError, syntax) = parse "12 * 3 + 65 * 4" rulesCtx
    putStrLn (concatMap withParentheses syntax)
    putStrLn ("Errors: " ++ show maybeError)
