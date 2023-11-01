{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}

module Main where

import Control.Applicative
import Data.Char (digitToInt, isDigit, isSpace)
import Prelude hiding ((>>=))

-- grammer
data JsonValue
  = JsonNull
  | JsonBool Bool
  | JsonNumber Int
  | JsonString String
  | JsonObject [(String, JsonValue)]
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

satisfy :: (Char -> Bool) -> Parser Char
satisfy predicate = Parser parseIfSatisfy
  where
    parseIfSatisfy (x : xs) = if predicate x then Just (x, xs) else Nothing
    parseIfSatisfy _ = Nothing

-- many applys parser till it fails
sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = (:) <$> element <*> many (sep *> element) <|> pure []

surroundedBy :: Parser a -> Parser b -> Parser a
surroundedBy p1 p2 = p2 *> p1 <* p2

charParser :: Char -> Parser Char
charParser toMatch = satisfy (== toMatch)

stringParser :: String -> Parser String
stringParser = traverse charParser

ws :: Parser String
ws = many (charParser ' ' <|> charParser '\n' <|> charParser '\r' <|> charParser '\t')

-- (<$) :: JsonValue -> Parser String -> Parser JsonValue
jsonNull :: Parser JsonValue
jsonNull = JsonNull <$ stringParser "null"

-- (<$>) :: (String -> JsonValue) -> Parser String -> Parser JsonValue
jsonBool :: Parser JsonValue
jsonBool = f <$> (stringParser "true" <|> stringParser "false")
  where
    f :: String -> JsonValue
    f "true" = JsonBool True
    f "false" = JsonBool False
    f _ = undefined

-- (*>) :: Parser Char -> Parser String -> Parser String
-- (<*) :: Parser String-> Parser Char -> Parser String
stringLiteral :: Parser String
stringLiteral = ((many . satisfy) (/= '"')) `surroundedBy` charParser '"'

jsonString :: Parser JsonValue
jsonString = JsonString <$> stringLiteral

jsonNumber :: Parser JsonValue
jsonNumber = (\ds -> JsonNumber $ read ds) <$> (some . satisfy) isDigit

jsonArray :: Parser JsonValue
jsonArray =
  JsonArray
    <$> ( charParser '['
            *> (elements `surroundedBy` ws)
            <* charParser ']'
        )
  where
    elements = sepBy (charParser ',' `surroundedBy` ws) jsonValue

jsonObject :: Parser JsonValue
jsonObject =
  JsonObject
    <$> ( charParser '{'
            *> (sepBy (charParser ',' `surroundedBy` ws) element) `surroundedBy` ws
            <* charParser '}'
        )
  where
    element =
      (\key _ val -> (key, val))
        <$> stringLiteral
        <*> (charParser ':' `surroundedBy` ws)
        <*> jsonValue

jsonValue :: Parser JsonValue
jsonValue =
  jsonNull
    <|> jsonBool
    <|> jsonString
    <|> jsonNumber
    <|> jsonArray
    <|> jsonObject

parseJson :: String -> Maybe JsonValue
parseJson s = case parse jsonValue s of
  Just (op, "") -> Just op
  _ -> Nothing

main :: IO ()
main = do
  let jsonData = "{\"a\": false, \"c\": null, \"foo\": [\"bar\",1,2,3,{}]}"
  case parseJson jsonData of
    Just parsedValue -> print parsedValue
    Nothing -> putStrLn "Failed to parse JSON"
