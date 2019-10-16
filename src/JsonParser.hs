module JsonParser where

import Control.Monad (void)
import Control.Monad.Combinators.Expr -- from parser-combinators
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import ParserTools

data JsonValue = JsonNull | JsonBool Bool | JsonArray [JsonValue] deriving (Show, Eq)

parseNull :: Parser JsonValue
parseNull = do
    sym "null"
    return JsonNull

parseTrue :: Parser JsonValue
parseTrue = do
    sym "true"
    return (JsonBool True)

parseFalse :: Parser JsonValue
parseFalse = do
    sym "false"
    return (JsonBool False)

parseBool :: Parser JsonValue
parseBool = parseTrue <|> parseFalse

parseArray :: Parser JsonValue
parseArray = do
    sym "["
    l <- sepBy parseValue (sym ",")
    sym "]"
    return (JsonArray l)

parseValue :: Parser JsonValue
parseValue = parseNull <|> parseBool <|> parseArray

parseJson :: Parser JsonValue
parseJson = do
    json <- parseValue
    eof
    return json


