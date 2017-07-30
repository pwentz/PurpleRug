module Main where

import Control.Monad
import Data.Char as Char
import Data.List as List
import Initializers.FormattedWords
import Initializers.Headers
import Initializers.OrderedList
import Initializers.Paragraphs
import Initializers.UnorderedList
import System.Environment
import System.IO

main :: IO ()
main = do
    args <- getArgs
    contents <- readFile (List.head args)
    writeFile "./output.html" $
        concatHtml . (List.map parse) . concatWrappedLines . lines $ contents

concatHtml :: [String] -> String
concatHtml elts = List.foldr concatWithBreaks "" elts
  where
    concatWithBreaks str acc = str ++ ("\n\n" ++ acc)

parse :: String -> String
parse text =
    case validResults of
        [] -> defaultFormat text
        ((Just x):_) -> x
  where
    parsers = [parseHeaders, parseList]
    results = List.map (\x -> x text) parsers
    validResults = List.filter ((/=) Nothing) results
    defaultFormat =
        (getParagraphs . paragraphs . getFormattedWords . formattedWords)

concatWrappedLines :: [String] -> [String]
concatWrappedLines =
    (List.map (List.dropWhileEnd Char.isSpace)) . (List.foldr concatWrapped [])
  where
    concatWrapped "" acc = "" : acc
    concatWrapped x [] = [x]
    concatWrapped x acc@(y:ys) = (x ++ ' ' : y) : ys

parseHeaders :: String -> Maybe String
parseHeaders line =
    case (headers line) of
        Nothing -> Nothing
        Just hs -> Just (getHeaders hs)

parseList :: String -> Maybe String
parseList line =
    case (unorderedList line) of
        Nothing ->
            case (orderedList line) of
                Nothing -> Nothing
                Just ordered -> Just (formatOrderedList ordered)
        Just unordered -> Just (formatUnorderedList unordered)
