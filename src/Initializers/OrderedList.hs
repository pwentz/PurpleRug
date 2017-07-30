module Initializers.OrderedList
    ( orderedList
    , getOrderedList
    , formatOrderedList
    ) where

import Data.Char as Char
import Data.List as List

newtype OrderedList = OrderedList
    { getOrderedList :: [String]
    } deriving (Show, Eq)

orderedList :: String -> Maybe OrderedList
orderedList text =
    if hasNumberPrefix text
        then Just (OrderedList (buildList text))
        else Nothing

hasNumberPrefix :: String -> Bool
hasNumberPrefix text =
    let (listNo, others) = List.span Char.isDigit text
    in (List.length listNo) > 0 && (List.isPrefixOf "." others)

formatOrderedList :: OrderedList -> String
formatOrderedList (OrderedList items) =
    let stringItems = foldr formatOrdered "</ol>" items
    in "<ol>\n" ++ stringItems
  where
    formatOrdered item acc = ("<li>" ++ item ++ "</li>") ++ '\n' : acc

buildList :: String -> [String]
buildList =
    (List.filter (/= "")) .
    (List.map (List.dropWhileEnd Char.isSpace)) . (List.foldr build []) . words
  where
    build str [] = [str]
    build str acc@(otherListItems:rest) =
        if (hasNumberPrefix str)
            then "" : acc
            else (str ++ (' ' : otherListItems)) : rest
