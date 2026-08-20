module Main where

import Control.Applicative
import Data.Char

data JsonValue
  = JsonNull
  | JsonBool Bool
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

instance Alternative Parser where
  empty = Parser $ \_ -> Nothing
  (Parser p1) <|> (Parser p2) = Parser $ \input ->
    (p1 input) <|> (p2 input)

charP :: Char -> Parser Char
charP x = Parser f
  where
    f (y : ys)
      | y == x = Just (ys, x)
      | otherwise = Nothing
    f [] = Nothing

stringP :: String -> Parser String
stringP = sequenceA . map charP

spanP :: (Char -> Bool) -> Parser String
spanP f = Parser $ \input ->
  let (token, rest) = span f input
   in Just (rest, token)

notEmpty :: Parser [a] -> Parser [a]
notEmpty (Parser p) =
  Parser $ \input -> case p input of
    Nothing -> Nothing
    Just (rest, x) | null x -> Nothing
    Just (rest, x) -> Just (rest, x)

-- Does not support escaping
stringLiteral :: Parser String
stringLiteral = charP '"' *> spanP (/= '"') <* charP '"'

ws :: Parser String
ws = spanP isSpace

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = (:) <$> element <*> many (sep *> element) <|> pure []

jsonNull :: Parser JsonValue
jsonNull = (\_ -> JsonNull) <$> stringP "null"

jsonBool :: Parser JsonValue
jsonBool = f <$> (stringP "true" <|> stringP "false")
  where
    f "true" = JsonBool True
    f "false" = JsonBool False
    -- This should never happen :)
    f _ = undefined

jsonNumber :: Parser JsonValue
jsonNumber = f <$> notEmpty (spanP isDigit)
  where
    f ds = JsonNumber $ read ds

jsonString :: Parser JsonValue
jsonString = JsonString <$> stringLiteral

jsonArray :: Parser JsonValue
jsonArray = JsonArray <$> (charP '[' *> ws *> elements <* ws <* charP ']')
  where
    elements = sepBy (ws *> charP ',' <* ws) jsonValue

jsonObject :: Parser JsonValue
jsonObject =
  JsonObject
    <$> ( charP '{'
            *> ws
            *> sepBy (ws *> charP ',' <* ws) pair
            <* ws
            <* charP '}'
        )
  where
    pair =
      (\key _ value -> (key, value)) <$> stringLiteral <*> (ws *> charP ':' *> ws) <*> jsonValue

jsonValue :: Parser JsonValue
jsonValue = jsonNull <|> jsonBool <|> jsonNumber <|> jsonString <|> jsonArray <|> jsonObject

parseFile :: FilePath -> Parser a -> IO (Maybe a)
parseFile filename parser = do
  input <- readFile filename
  return (snd <$> runParser parser input)

main :: IO ()
main = undefined
