module Initializers.Headers
    ( headers
    , getHeaders
    ) where

import Data.List as List

newtype Headers = Headers
    { getHeaders :: String
    } deriving (Show, Eq)

headers :: String -> Maybe Headers
headers x
    | hSize == 0 || hSize > 6 || head msg /= ' ' = Nothing
    | otherwise =
        Just
            (Headers
                 ("<h" ++
                  show hSize ++ ">" ++ tail msg ++ "</h" ++ show hSize ++ ">"))
  where
    isPrefix = (== '#')
    hSize = (List.length . List.takeWhile isPrefix) x
    msg = List.dropWhile isPrefix x
