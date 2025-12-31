{-
    This file defines two data structures:
    Trie and TokenMap.
-}
module Structures where

import Data.Map (Map)
import qualified Data.Map as Map

{-
    Here we define a trie data structure. This is used to store the list
    of tokens. By storing the tokens as a trie, we can easily extract
    the longest matching token.
-}

data Trie a = Trie (Maybe a) [(Char, Trie a)]
    deriving (Show, Eq)

-- Construct an empty trie
empty :: Trie a
empty = Trie Nothing []

-- Insert the value associated with the string into the trie
insert :: String -> a -> Trie a -> Trie a
insert [] val (Trie _ children) = Trie (Just val) children
insert (x : xs) val (Trie v children) =
    Trie v (insertChild x xs val children)
    where
    insertChild char remChars value [] = [(char, insert remChars value empty)]
    insertChild char remChars value ((k, y) : ys)
        | char == k = (k, insert remChars value y) : ys
        | otherwise = (k, y) : insertChild char remChars value ys

{-
    Look for the key in the trie that is the longest prefix of
    the string starting at the given index. Then return the associated value.
-}
matchPrefix :: String -> Trie a -> Int -> Maybe a
matchPrefix s trie i = go trie i Nothing
    where
    go (Trie val children) idx bestMatch =
        let currentMatch = case val of
                            Just _ -> val
                            Nothing -> bestMatch in
        if idx >= length s then
            currentMatch
        else
            let char = s !! idx in
            case lookup char children of
                Nothing -> currentMatch
                Just childTrie -> go childTrie (idx + 1) currentMatch

{-
    Find the value of the given key.
-}
maybeFind :: String -> Trie a -> Maybe a
maybeFind [] (Trie v _) = v
maybeFind (x : xs) (Trie _ children) =
    case findChild x children of
    Nothing -> Nothing
    Just child -> maybeFind xs child
    where
        findChild :: Char -> [(Char, Trie a)] -> Maybe (Trie a)
        findChild _ [] = Nothing
        findChild x ((v, c) : cs) =
            if x == v then
                Just c
            else
                findChild x cs

{-
    Here we define a dictionary structure. It maps the name of a token to
    the list parsers that can parse it.
-}
newtype TokenMap a = TokenMap (Map String [a])
    deriving Show

{-
    Add a parser to the list of parsers that can parse a given token.
-}
insertTokenMap :: TokenMap a -> String -> a -> TokenMap a
insertTokenMap (TokenMap map) key value =
    TokenMap $
    Map.insertWithKey
        (\_ _ oldValue -> value : oldValue) 
        key [value] map

{-
    Get the list of parsers that can parse a given token.
-}
lookupTokenMap :: TokenMap a -> String -> Maybe [a]
lookupTokenMap (TokenMap map) key = Map.lookup key map

emptyTokenMap :: TokenMap a
emptyTokenMap = TokenMap Map.empty