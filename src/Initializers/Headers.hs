module Initializers.Headers
    ( headers
    , getHeaders
    ) where

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
    hSize = (length . takeWhile isPrefix) x
    msg = dropWhile isPrefix x
