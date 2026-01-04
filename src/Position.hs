{-# LANGUAGE ScopedTypeVariables #-}

{-
    Defines `toPosition` which turns an index of the input string
    into a line and column position in the input.
-}
module Position where

{-
    Records a position in the source input.

    This is used to implement things like `checkColGt`
    to ensure that text is indented.
-}
data Position = Position {
    line :: Int,
    column :: Int
}
    deriving (Show, Eq)

{-
    Stores the locations of newlines in the input string.

    This is used to quickly convert a position in the input string
    to a `Position`.

    `positions` has its first entry as `0` and its last entry the index of the
    last character. So the set of all indices in a line is always between two
    consecutive entries of the positions list.
-}
data FileMap = FileMap {
    source :: String,
    positions :: [Int]
}
    deriving (Show, Eq)

{-
    Store the positions of all the newlines in the given string.
    It also stores the positions 0 and `length s`.
-}
fileMapOfString :: String -> FileMap
fileMapOfString s = go 0 [0]
    where
        go :: Int -> [Int] -> FileMap
        go currPos ps =
            if currPos >= length s then
                FileMap {source = s, positions = reverse (currPos : ps)}
            else
                let currChar = s !! currPos in
                if currChar == '\n' then
                    go (currPos + 1) (currPos : ps)
                else
                    go (currPos + 1) ps

{-
    Find the largest index `i` such that `ls[i] ≤ x`.
    Returns `length ls - 1` if there isn't one.
    Assumes that the given list is sorted in ascending order.
-}
largestLE :: forall a. Ord a => a -> [a] -> Int
largestLE x ls = go 0 (length ls - 1) ls
    where
        go :: Int -> Int -> [a] -> Int
        go _ best [] = best
        go idx best (y : ys)
            | y <= x = go (idx + 1) idx ys
            | otherwise = best
{-
    Convert an index of the input string into a line and column `Position`.
-}
toPosition :: FileMap -> Int -> Position
toPosition (FileMap source ps) pos =
    let 
        toColumn :: Int -> Int -> Int
        toColumn idx column =
            if idx == pos || idx >= length source then
                column
            else
                toColumn (idx + 1) (column + 1)
        lineNum = largestLE pos ps
        lineStartIdx = ps !! lineNum
        columnNum = toColumn lineStartIdx 0 in
    Position {line = lineNum, column = columnNum}

