-- Module for pretty printing tables
-- Available publically
-- Source: https://codereview.stackexchange.com/questions/171992/pretty-printed-tables-in-haskell
-- Author: Phil Kiener
-- Last access date: 26.04.2021
module Tablefy
    ( tablefy
    ) where

import Prelude hiding (Left, Right)
import Data.List (intercalate, intersperse, transpose)

-- | Constructs a pretty table. @tablefy h rs@ constructs a table using the
--   elements of @h@ as headers and each list in @rs@ as row. If any list in
--   @rs@ has a length different than the length from @h@, this function throws
--   an error.
--
--   The table is returned in the following format:
--
--   @
--     +--------+--------+--------+
--     | HEADER | HEADER | HEADER |
--     +--------+--------+--------+
--     | CELL   | CELL   | CELL   |
--     +--------+--------+--------+
--     | CELL   | CELL   | CELL   |
--     +--------+--------+--------+
--     | CELL   | CELL   | CELL   |
--     +--------+--------+--------+
--   @
--
--   Header cells are centrally padded, data cells are left padded; both use
--   a single space as fill character.
tablefy :: [String] -> [[String]] -> String
tablefy h rs
    | any (/= length h) (map length rs) = error "Tablefy.tablefy: Differences in length"
    | otherwise                         = table
    where
        table  = unlines $ insert' sep (header:rows)
        widths = map (maximum . map length) (transpose (h:rs))
        sep    = insert "+" $ map (flip replicate '-' . (+2)) widths
        header = mkRow Center h
        rows   = map (mkRow Left) rs
        mkRow a       = insert "|" . zipWith (mkCell a) widths
        mkCell a n xs = " " ++ pad a n ' ' xs ++ " "

-- | @insert x xs@ prepends and appends @x@ to the result of @intercalate x xs@.
--
--   >>> insert "#" ["Alpha","Beta","Gamma"]
--   "#Alpha#Beta#Gamma#"
insert :: [a] -> [[a]] -> [a]
insert x xs = intercalate x ([] : xs ++ [[]])

-- | Version of 'insert' that uses 'intersperse' instead of 'intercalate'.
--
--   >>> insert' "#" ["Alpha","Beta","Gamma"]
--   >>> ["#", "Alpha", "#", "Beta", "#", "Gamma", "#"]
insert' :: [a] -> [[a]] -> [[a]]
insert' x xs = intersperse x ([] : xs ++ [[]])

-- | Alignment is a simple sum type containing the different modes of padding.
data Alignment = Left | Right | Center deriving Eq

-- | @pad a n x xs@ pads the given list @xs@ using the element @x@ to make sure
--  it has length @n@. Exceeding elements are cut off. If @a@ is 'Left', the
--  fill element is appended. If @a@ is 'Right', the fill element is prepended.
--  If @a@ is 'Center', the fill element is both appended and prepended. If an
--  odd number of fills needs to be inserted, there will be one more element
--  appended than there will be prepended.
--
--  >>> pad Center 6 ' ' "Hello"
--  "Hello "
--
--  >>> pad Center 9 '-' "Hello"
--  "--Hello--"
--
--  >>> pad Right 3 'x' "Hello"
--  "Hel"
pad :: Alignment -> Int -> a -> [a] -> [a]
pad a n x xs
    | n < 1          = error "Tablefy.pad: Length must not be smaller than one"
    | n <= length xs = take n xs
    | a == Left      = xs ++ replicate (n - length xs) x
    | a == Right     = replicate (n - length xs) x ++ xs
    | a == Center    = let (space, extra) = quotRem (n - length xs) 2
                       in replicate space x ++ xs ++ replicate (space + extra) x