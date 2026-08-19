module Main where

import Data.Char

data JsonValue
  = JsonNull
  | JsonBool
  | JsonNumber Integer
  | JsonString String
  | JsonArray [JsonValue]
  | JsonObject [(String, JsonValue)]
  deriving (Show, Eq)

newtype Parser a = Parser
  { runParser :: String -> Maybe (String, a)
  }

instance Functor Parser where
  fmap f (Parser p) = Parser $ \input -> case p input of
    Nothing -> Nothing
    Just (rest, x) -> Just (rest, f x)

instance Applicative Parser where
  pure x = Parser $ \input -> Just (input, x)
  (Parser p1) <*> (Parser p2) = Parser $ \input ->
    case p1 input of
      Nothing -> Nothing
      Just (r1, f) -> case p2 r1 of
        Nothing -> Nothing
        Just (r2, x) -> Just (r2, f x)

jsonNull :: Parser JsonValue
jsonNull = undefined

charP :: Char -> Parser Char
charP x = Parser f
  where
    f (y : ys)
      | y == x = Just (ys, x)
      | otherwise = Nothing
    f [] = Nothing

-- stringP :: String -> Parser String
-- stringP = sequenceA . map charP

main :: IO ()
main = undefined
