module PurpleRug.Internal
  (
    headers
  , paragraphs
  , formatWords
  , unorderedLists
  , orderedLists
  , splitEvery
  , splitBy
  ) where

import Data.List
import Data.Char

headers :: String -> String
headers x
  | hSize == 0 || hSize > 6 || head msg /= ' ' = x
  | otherwise = "<h" ++ show hSize ++ ">" ++ tail msg ++ "</h" ++ show hSize ++ ">"
  where isPrefix = (=='#')
        hSize = (length . takeWhile isPrefix) x
        msg = dropWhile isPrefix x

wrapPTags :: String -> String -> String
wrapPTags s acc
    | s == "" = "</p>\n<p>" ++ acc
    | take 4 acc == "</p>" = s ++ acc
    | otherwise = s ++ ' ':acc

replaceWithBreaks :: String -> String -> String
replaceWithBreaks pattern = intercalate "\n" . filter (/=pattern) . lines

paragraphs :: String -> String
paragraphs "" = ""
paragraphs s = ("<p>"++) . replaceWithBreaks "<p></p>" . foldr wrapPTags "</p>" . lines $ s

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (x, y) = (f x, f y)

formatWords :: String -> String
formatWords = emphasize ("**", "strong") . emphasize ("*", "em")

asTags tag = ("<" ++ tag ++ ">", "</" ++ tag ++ ">")

emphasize format@(sym, tag) s
    | h == s = s
    | otherwise = let (open, close) = asTags tag
                      (h1, t1) = splitAtSym $ replaceFirst h open t
                      in emphasize format $ replaceFirst h1 close t1
  where replaceFirst h pos t = h ++ (pos ++ drop (length sym) t)
        splitAtSym = mapTuple concat . span (/=sym) . group
        (h,t) = splitAtSym s

consDiff :: [a] -> [[a]] -> [[a]]
consDiff x acc = (take (length x - (sum $ map length acc)) x):acc

charsRedacted :: String -> String -> [String]
charsRedacted pattern "" = []
charsRedacted pattern s
    | (isPrefixOf pattern s) = [""] ++ charsRedacted pattern (drop (length pattern) s)
    | otherwise = (take 1 s):(charsRedacted pattern $ tail s)

splitEvery :: String -> String -> [String]
splitEvery pattern "" = []
splitEvery pattern s = [concat nextChunk] ++ splitEvery pattern rest
  where chars = charsRedacted pattern s
        nextChunk = takeWhile (/="") chars
        rest = drop (length nextChunk + length pattern) s

-- splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy pred = filter (/="") . map (dropWhile pred) . foldr consDiff [] . filter (pred . head) . init . tails

wrapListItem s acc = "\n<li>" ++ s ++ "</li>" ++ acc

unorderedLists :: String -> String
unorderedLists = ("<ul>"++) . foldr wrapListItem "\n</ul>" . map (dropWhileEnd isSpace) . filter (/="") . splitEvery "* "

orderedLists :: String -> String
orderedLists = ("<ol>"++) . foldr wrapListItem "\n</ol>" . filter (/="") . splitBy pred
  where pred x = isDigit x || x == '.' || isSpace x
