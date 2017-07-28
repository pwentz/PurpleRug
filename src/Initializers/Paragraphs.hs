module Initializers.Paragraphs
    ( paragraphs
    , getParagraphs
    ) where

import Data.List as List

newtype Paragraphs = Paragraphs
    { getParagraphs :: String
    }

wrapPTags :: String -> String -> String
wrapPTags s acc
    | s == "" = "</p>\n<p>" ++ acc
    | take 4 acc == "</p>" = s ++ acc
    | otherwise = s ++ ' ' : acc

replaceWithBreaks :: String -> String -> String
replaceWithBreaks pattern =
    List.intercalate "\n" . List.filter (/= pattern) . List.lines

paragraphs :: String -> Paragraphs
paragraphs "" = Paragraphs ""
paragraphs s = Paragraphs (makeParagraphs s)
  where
    makeParagraphs =
        ("<p>" ++) .
        replaceWithBreaks "<p></p>" . List.foldr wrapPTags "</p>" . List.lines
