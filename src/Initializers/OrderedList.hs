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
orderedList text =
    let (listNo, others) = List.span Char.isDigit text
    in if ((List.length listNo) > 0 && (List.isPrefixOf "." others))
           then Just (OrderedList (buildList text))
           else Nothing

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
        let (listNo, others) = List.span Char.isDigit str
        in if ((List.length listNo) > 0 && (List.isPrefixOf "." others))
               then "" : acc
               else (str ++ (' ' : otherListItems)) : rest
