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

splitByDigit :: String -> (String, String)
splitByDigit = List.span Char.isDigit

isNumberFormat :: (String, String) -> Bool
isNumberFormat (h, '.':' ':_) = length h > 0
isNumberFormat (h, t) = False

appendItems :: [String] -> String -> [String]
appendItems [] x = [x]
appendItems acc@(y:ys) x =
    if isNumberFormat (List.span Char.isDigit x)
        then acc ++ [x]
        else if (length ys == 0)
                 then [y ++ ('\n' : x)]
                 else (init acc) ++ [last acc ++ ('\n' : x)]

orderedList :: String -> Maybe OrderedList
orderedList s
    | isNumberFormat (splitByDigit s) =
        let listItems =
                map ((drop 2) . (dropWhile isDigit)) .
                foldl appendItems [] . lines $
                s
        in (Just (OrderedList listItems))
    | otherwise = Nothing

formatOrderedList :: OrderedList -> String
formatOrderedList (OrderedList items) =
    let stringItems = foldr formatOrdered "</ol>" items
    in "<ol>\n" ++ stringItems
  where
    formatOrdered item acc = ("<li>" ++ item ++ "</li>") ++ '\n' : acc
