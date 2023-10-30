module Main where

import Control.Applicative
import Data.HashMap.Strict qualified as HashMap
import Prelude

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
newtype Parser a = Parser {parse :: String -> Maybe (a, String)}

-- need to proove parser is functor
instance Functor Parser where
  -- fmap :: (a -> b) -> f a -> f b
  fmap f (Parser parser1) =
    Parser $ \ip -> do
      (x, ip1) <- parser1 ip
      Just (f x, ip1)

-- need to proove parser is applicative
instance Applicative Parser where
  -- pure :: a -> f a
  pure x = Parser $ \input -> Just (x, input)

  -- (<*>) :: f (a -> b) -> f a -> f b
  (Parser parser1) <*> (Parser parser2) =
    Parser $ \input -> do
      (f, input1) <- parser1 input
      (a, input2) <- parser2 input1
      Just (f a, input2)

-- need to proove parser is alternative
instance Alternative Parser where
  -- empty :: f a
  empty = Parser $ const Nothing

  -- (<|>) :: f a -> f a -> f a
  (Parser parser1) <|> (Parser parser2) = Parser $ \input -> do
    parser1 input <|> parser2 input

charParser :: Char -> Parser Char
charParser toMatch = Parser parser
  where
    parser (toCheck : rest)
      | toCheck == toMatch = Just (toCheck, rest)
      | otherwise = Nothing

stringParser :: String -> Parser String
stringParser = traverse charParser

nullParser :: Parser JsonValue
nullParser = JsonNull <$ stringParser "null"

boolParser :: Parser JsonValue
boolParser = f <$> (stringParser "true" <|> stringParser "false")
  where
    f "true" = JsonBool True
    f "false" = JsonBool False
    f _ = undefined

main :: IO ()
main = putStrLn "Hello, Haskell!"
