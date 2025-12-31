{-
    This file defines the parsers `leadingNode` and `trailingNode`
    which each turn a parser into one that has precedence and can be used
    by the Pratt parser.
-}
module Pratt.PrecParsers where

import Defs
import Parsers.Basic
import Parsers.Token

{-
    Checks that the given precedence is larger
    than the that in the context.
-}
checkPrecFn :: Int -> ParserFn
checkPrecFn precInt c s =
    if prec c <= precInt then
        s
    else
        mkError s "unexpected token at precedence level"

checkPrec :: Int -> Parser
checkPrec prec = Parser {
    info = ParserInfo {
        collectTokens = id,
        collectKinds = id
    },
    fn = checkPrecFn prec
}

{-
    Convert the top elements of syntax in the parser state
    into a single syntax node.
-}
mkNode :: ParserState -> SyntaxNodeKind -> Int -> ParserState
mkNode s k initialSize =
    let currentSize = length (syntax s) in
    {-
        If there is an error but no node has been added,
        add a missing node.
    -}
    if hasError s && currentSize == initialSize then
        pushSyntax s Missing
    else
        let lengthDiff = currentSize - initialSize
            children = extractSublist (syntax s) 0 lengthDiff
            newNode = Node k children
            syntaxListWithoutChildren = drop lengthDiff (syntax s)
            newSyntaxList = newNode : syntaxListWithoutChildren in
        s {syntax = newSyntaxList}

{-
    Convert the top elements (plus one!) of syntax in the parser state
    into a single syntax node.
-}
mkTrailingNode :: ParserState -> SyntaxNodeKind -> Int -> ParserState
mkTrailingNode s k initialSize =
    let currentSize = length (syntax s) + 1
        lengthDiff = currentSize - initialSize
        children = extractSublist (syntax s) 0 lengthDiff
        newNode = Node k children
        syntaxListWithoutChildren = drop lengthDiff (syntax s)
        newSyntaxList = newNode : syntaxListWithoutChildren in
    s {syntax = newSyntaxList} 

{-
    Runs the given parser and then puts all the
    syntax it created into a single node.
-}
nodeFn :: SyntaxNodeKind -> ParserFn -> ParserFn
nodeFn kind p c s =
    let initialSize = length (syntax s)    
        new_s = p c s in
    mkNode new_s kind initialSize

nodeInfo :: SyntaxNodeKind -> ParserInfo -> ParserInfo
nodeInfo kind p = ParserInfo {
    collectTokens = collectTokens p,
    collectKinds = \s -> kind : collectKinds p s
}

{-
    Runs the parser and then collects the resulting
    syntax into a single node.
-}
node :: SyntaxNodeKind -> Parser -> Parser
node kind p = Parser {
    fn = nodeFn kind (fn p),
    info = nodeInfo kind (info p)
}

{-
    Runs the parser and then collects the resulting
    syntax as well as one more into a single node.
-}
trailingNodeFn :: SyntaxNodeKind -> ParserFn -> ParserFn
trailingNodeFn kind p c s =
    let initialSize = length (syntax s)    
        new_s = p c s in
    mkTrailingNode new_s kind initialSize

{-
    Like the `node` parser but it also collects
    one syntax that existed before running the given parser.
    This is the left hand side created from the leading parser
    or from previous trailing parsers.
-}
trailingNodeAux :: SyntaxNodeKind -> Parser -> Parser
trailingNodeAux kind p = Parser {
    fn = trailingNodeFn kind (fn p),
    info = nodeInfo kind (info p)
}

{-
    Set the `lhsPrec` in the parser state to the
    given precedence.
-}
setLhsPrecFn :: Int -> ParserFn
setLhsPrecFn precInt _ s =
    if hasError s then
        s
    else
        s {lhsPrec = precInt}

setLhsPrec :: Int -> Parser
setLhsPrec precInt = Parser {
    info = ParserInfo {
        collectTokens = id,
        collectKinds = id
    },
    fn = setLhsPrecFn precInt
}

checkLhsPrecFn :: Int -> ParserFn
checkLhsPrecFn prec _ s =
    if lhsPrec s >= prec then
        s
    else
        mkError s "unexpected token at this precedence level"

checkLhsPrec :: Int -> Parser
checkLhsPrec prec = Parser {
    info = ParserInfo {
        collectTokens = id,
        collectKinds = id
    },
    fn = checkLhsPrecFn prec
}

{-
    Given a precedence `prec` and a parser `p`,
    create a new parser that has the given precedence
    and combines all the resulting syntax made by `p` into a single node.
    This allows it to be used by the pratt parser properly.

    Check that the given precedence `prec` is larger than that in the context.
    Run the parser `node` with `p` as argument.
    Set the lhs precedence of the previously run parser to be `prec` since
    the previously run parser is now `p`.
-}
leadingNode :: SyntaxNodeKind -> Int -> Parser -> Parser
leadingNode n prec p =
    checkPrec prec `andthen` node n p `andthen` setLhsPrec prec 

{-
    Given a precedence `prec`, precedence `lhsPrec`, and parser `p`,
    create a new parser that has the given precedence and `lhsPrec` restriction.

    Check that the given precedence `prec` is larger than that in the context.
    Check that the given `lhsPrec` is smaller than that in the parser state.
    Run the parser `trailingNodeAux` with `p` as argument.
    Set the lhs precedence of the previously run parser to be `prec` since
    the previously run parser is now `p`.
-}
trailingNode :: SyntaxNodeKind -> Int -> Int -> Parser -> Parser
trailingNode kind prec lhsPrec p =
    checkPrec prec `andthen` checkLhsPrec lhsPrec `andthen`
        trailingNodeAux kind p `andthen` setLhsPrec prec