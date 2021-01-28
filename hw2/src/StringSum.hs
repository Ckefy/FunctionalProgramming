module StringSum where

import Data.Char
import Text.Read

-- | Split string into list of words (split on whitespaces)
splitter :: String -> [String]
splitter input =  case dropWhile isSpace input of
                      "" -> []
                      input' -> word : splitter inputAfter
                            where (word, inputAfter) = break isSpace input'

--splitter returns list of String
--readMaybe String -> Maybe Int
--traverse map readMaybe to every element of list
--traverse (String -> Maybe Int) -> List String -> Maybe (List Int)
-- | Sum of integer numbers in list
-- | If some fails, then the whole result in Nothing
stringSum :: String -> Maybe Int
stringSum input = sum <$> traverse readMaybe (splitter input)
