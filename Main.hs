module Main where

import Prelude
import qualified Data.HashMap.Strict as HashMap

-- grammer
data JsonValue
  = JsonNull
  | JsonBool Bool
  | JsonNumber Integer
  | JsonString String
  | JsonObject (HashMap.HashMap String JsonValue)
  | JsonArray [JsonValue]
  | JsonValue
  deriving (Show, Eq)

-- input -> Maybe (remaining input, parsed value)
type Parser a = String -> Maybe (String, a)

charParser :: Char -> Parser Char
charParser x = parser
        where 
            parser (y:ys) 
                | x == y = Just (ys ,x)
                | otherwise = Nothing

-- stringParser :: String -> Parser String
-- stringParser (x:xs) = (charParser 'a')

main :: IO ()
main = putStrLn "Hello, Haskell!"
