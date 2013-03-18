{-# LANGUAGE RankNTypes, FlexibleContexts, NoMonomorphismRestriction #-}
module Parser where

import Text.Parsec
import Control.Applicative (pure, (<$>), (<$), (<*>), (<*), (*>))
import Control.Monad.Identity
import Lexer
import Hisp

type Parser a = Stream [Token] m Token => ParsecT [Token] u m a

number = Number <$> lNumber
var = Variable <$> lIdentifier
lambda = Lambda <$ lLambda <*> lIdentifier <* lArrow <*> hispExpr
atom = choice $ map try [number, var, lambda]
application = chainl1 atom $ pure Application
hispExpr = application

parser :: Parser HispExpr
parser = hispExpr <* eof

parseHisp :: String -> Either ParseError HispExpr
parseHisp s = runLexer s >>= runParser parser () ""
