module Initializers.UnorderedList
    ( unorderedList
    , getUnorderedList
    , formatUnorderedList
    , buildList
    ) where

import Data.Char as Char
import Data.List as List

newtype UnorderedList = UnorderedList
    { getUnorderedList :: [String]
    } deriving (Show, Eq)

unorderedList :: String -> Maybe UnorderedList
unorderedList text@('*':' ':_) = Just (UnorderedList (buildList text))
unorderedList _ = Nothing

formatUnorderedList :: UnorderedList -> String
formatUnorderedList (UnorderedList items) =
    let stringItems = foldr formatUnordered "</ul>" items
    in "<ul>\n" ++ stringItems
  where
    formatUnordered item acc = ("<li>" ++ item ++ "</li>") ++ '\n' : acc

buildList :: String -> [String]
buildList =
    (List.filter (/= "")) .
    (List.map (List.dropWhileEnd Char.isSpace)) . (List.foldr build []) . words
  where
    build "*" acc = "" : acc
    build str [] = [str]
    build str (otherListItems:rest) = (str ++ (' ' : otherListItems)) : rest
