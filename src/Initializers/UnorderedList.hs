module Initializers.UnorderedList
    ( unorderedList
    , getUnorderedList
    , formatUnorderedList
    ) where

import Data.List as List

newtype UnorderedList = UnorderedList
    { getUnorderedList :: [String]
    } deriving (Show, Eq)

appendItems [] x = [x]
appendItems acc@(y:ys) x =
    if List.isPrefixOf "* " x
        then acc ++ [x]
        else if (length ys == 0)
                 then [y ++ ('\n' : x)]
                 else (init acc) ++ [last acc ++ ('\n' : x)]

unorderedList :: String -> Maybe UnorderedList
unorderedList s@('*':' ':_) =
    let listItems = map (drop 2) . foldl appendItems [] . lines $ s
    in (Just (UnorderedList listItems))
unorderedList _ = Nothing

formatUnorderedList :: UnorderedList -> String
formatUnorderedList (UnorderedList items) =
    let stringItems = foldr formatUnordered "</ul>" items
    in "<ul>\n" ++ stringItems
  where
    formatUnordered item acc = ("<li>" ++ item ++ "</li>") ++ '\n' : acc
