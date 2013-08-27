module Lexer (
    Token
  , lLParen
  , lRParen
  , lLambda
  , lDot
  , lEqual
  , lIdentifier
  , lNumber
  , lexer
  , runLexer
) where

import MakeParsers
import Text.Parsec
import Control.Applicative ((<$>), (<$), (<*>), (<*), (*>))
import Control.Monad.Identity

data Token = LParen
           | RParen
           | Lambda
           | Dot
           | Equal
           | Identifier String
           | Number Int
           deriving (Show, Eq, Ord)

type Lexer a = Stream String m Char => ParsecT String u m a

lparen = LParen <$ char '(' <?> "opening parenthesis"
rparen = RParen <$ char ')' <?> "closing parenthesis"
lambda = Lambda <$ char '\\' <?> "lambda"
dot = Dot <$ string "." <?> "dot"
equal = Equal <$ char '=' <?> "equal sign"
identifier = Identifier <$> many1 (letter <|> char '_') <?> "identifier"
number = Number . read <$> many1 digit <?> "number"

lexOne = choice [lparen, rparen, lambda, dot, identifier, number] <* spaces

lexer :: Lexer [Token]
lexer = spaces *> many lexOne <* eof

runLexer :: String -> Either ParseError [Token]
runLexer = runParser lexer () ""

mkLs ''Token
