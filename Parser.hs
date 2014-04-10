module Parser where

import Text.Parsec
import Control.Applicative (pure, (<$>), (<$), (<*>), (<*), (*>))
import Lexer
import Hisp

type Parser a = Stream [Token] m Token => ParsecT [Token] u m a

pNumber, pVar, pLambda, pParenthesised, pAtom, pApplication, pHispExpr :: Parser (HispExpr String)
pNumber = Number <$> lNumber
pVar = Variable <$> lIdentifier
pLambda = flip (foldr lambda) <$ lLambda <*> many1 lIdentifier <* lDot <*> pHispExpr
pParenthesised = lLParen *> pHispExpr <* lRParen
pAtom = choice [pNumber, pVar, pLambda, pParenthesised]
pApplication = chainl1 pAtom $ pure (:@)
pHispExpr = pApplication

parser :: Parser (HispExpr String)
parser = pHispExpr <* eof

parseHisp :: String -> Either ParseError (HispExpr String)
parseHisp s = runLexer s >>= runParser parser () ""
