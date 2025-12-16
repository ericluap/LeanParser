module Trie where

{-
    This file defines a trie data structure. This is used to store the list
    of tokens. By storing the tokens as a trie, we can easily extract
    the longest matching token.
-}

data Trie a = Trie (Maybe a) [(Char, Trie a)]

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
            