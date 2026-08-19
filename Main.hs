module Main where

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

jsonNull :: Parser JsonValue
jsonNull = undefined

charP :: Char -> Parser Char
charP x = Parser $ \input ->
  case input of
    y : ys | y == x -> Just (ys, x)
    _ -> Nothing

main :: IO ()
main = undefined
