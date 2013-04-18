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

type Lexer a = Stream [Char] m Char => ParsecT [Char] u m a

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

lIs tok err = try $ anyToken >>= \x -> if x == tok then return () else fail err

lLParen = lIs LParen "left parenthesis"
lRParen = lIs RParen "right parenthesis"
lLambda = lIs Lambda "lambda"
lDot  = lIs Dot "dot"
lEqual = lIs Equal "equal"

lIdentifier = try $ anyToken >>= \x -> case x of
    Identifier x' -> return x'
    _ -> fail "identifier"

lNumber = try $ anyToken >>= \x -> case x of
    Number x' -> return x'
    _ -> fail "number"

