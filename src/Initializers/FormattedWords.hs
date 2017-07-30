module Initializers.FormattedWords
    ( formattedWords
    , getFormattedWords
    ) where

import Data.List as List

newtype FormattedWords = FormattedWords
    { getFormattedWords :: String
    } deriving (Show, Eq)

formattedWords :: String -> FormattedWords
formattedWords words = FormattedWords (format words)
  where
    format = emphasize ("**", "strong") . emphasize ("*", "em")

asTags :: String -> (String, String)
asTags tag = ("<" ++ tag ++ ">", "</" ++ tag ++ ">")

emphasize :: (String, String) -> String -> String
emphasize format@(formatting, tag) text
    | head == text = text
    | otherwise =
        let (open, close) = asTags tag
            (h1, t1) = splitAtFormatting $ replaceFirst head open tail
        in emphasize format $ replaceFirst h1 close t1
  where
    replaceFirst head tag tail =
        head ++ (tag ++ List.drop (List.length formatting) tail)
    splitAtFormatting =
        mapTuple List.concat . List.break (== formatting) . List.group
    (head, tail) = splitAtFormatting text

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (x, y) = (f x, f y)
