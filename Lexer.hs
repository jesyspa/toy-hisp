{-# LANGUAGE RankNTypes, FlexibleContexts, NoMonomorphismRestriction #-}
module Lexer (
    Token
  , lLParen
  , lRParen
  , lLambda
  , lArrow
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
           | Arrow
           | Equal
           | Identifier String
           | Number Int
           deriving (Show, Eq, Ord)

type Lexer a = Stream [Char] m Char => ParsecT [Char] u m a

lparen = LParen <$ char '(' <?> "opening parenthesis"
rparen = RParen <$ char ')' <?> "closing parenthesis"
lambda = Lambda <$ char '\\' <?> "lambda"
arrow = Arrow <$ string "->" <?> "arrow"
equal = Equal <$ char '=' <?> "equal sign"
identifier = Identifier <$> many1 letter <?> "identifier"
number = Number . read <$> many1 digit <?> "number"

lexOne = choice [lparen, rparen, lambda, arrow, identifier, number] <* spaces

lexer :: Lexer [Token]
lexer = spaces *> many lexOne <* eof

runLexer :: String -> Either ParseError [Token]
runLexer = runParser lexer () ""

lLParen = try $ anyToken >>= \x -> case x of
    LParen -> return ()
    _ -> fail "left parenthesis"

lRParen = try $ anyToken >>= \x -> case x of
    RParen -> return ()
    _ -> fail "right parenthesis"

lLambda = try $ anyToken >>= \x -> case x of
    Lambda -> return ()
    _ -> fail "lambda"

lArrow = try $ anyToken >>= \x -> case x of
    Arrow -> return ()
    _ -> fail "arrow"

lEqual = try $ anyToken >>= \x -> case x of
    Equal -> return ()
    _ -> fail "equal"

lIdentifier = try $ anyToken >>= \x -> case x of
    Identifier x' -> return x'
    _ -> fail "identifier"

lNumber = try $ anyToken >>= \x -> case x of
    Number x' -> return x'
    _ -> fail "number"

