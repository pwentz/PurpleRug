module Initializers.FormattedWords
    ( formattedWords
    , getFormattedWords
    ) where

import Data.List as List

newtype FormattedWords = FormattedWords
    { getFormattedWords :: String
    } deriving (Show, Eq)

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (x, y) = (f x, f y)

formattedWords :: String -> FormattedWords
formattedWords words = FormattedWords (format words)
  where
    format = emphasize ("**", "strong") . emphasize ("*", "em")

asTags :: String -> (String, String)
asTags tag = ("<" ++ tag ++ ">", "</" ++ tag ++ ">")

emphasize :: (String, String) -> String -> String
emphasize format@(sym, tag) s
    | h == s = s
    | otherwise =
        let (open, close) = asTags tag
            (h1, t1) = splitAtSym $ replaceFirst h open t
        in emphasize format $ replaceFirst h1 close t1
  where
    replaceFirst h pos t = h ++ (pos ++ List.drop (length sym) t)
    splitAtSym = mapTuple List.concat . List.span (/= sym) . List.group
    (h, t) = splitAtSym s
