module ExpParser where

import Control.Monad (void)
import Control.Monad.Combinators.Expr -- from parser-combinators
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import ParserTools

data Command = Let String String | Get String deriving Show

letCmd :: Parser Command
letCmd = do
    sym "let"
    key <- strLit
    sym "="
    val <- strLit
    return (Let key val)

getCmd :: Parser Command
getCmd = do
    sym "get"
    key <- strLit
    return (Get key)

command :: Parser Command
command = letCmd <|> getCmd

