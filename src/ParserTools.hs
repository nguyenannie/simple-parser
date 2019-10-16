module ParserTools where

import Control.Monad (void)
import Control.Monad.Combinators.Expr -- from parser-combinators
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser a = Parsec Void String a

spc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

lexm :: Parser a -> Parser a
lexm = L.lexeme spc

sym :: String -> Parser ()
sym symbolLiteral = void $ L.symbol spc symbolLiteral

strLit :: Parser String
strLit = lexm $ some alphaNumChar