{-# OPTIONS_GHC -fno-warn-unused-do-bind -fno-warn-unused-matches #-}
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
-- Peggy produces many spurrious warnings this way.
module Hisp.PeggyParser (
    peggyParse, showError
) where

import qualified Hisp.Hisp as H
import Hisp.Hisp (HExpr, UnifiedHExpr, (|@@|))
import Hisp.TypeLike
import Text.Peggy

type Type = ()

[peggy|

top :: Unification Type [(String, HExpr Type)]
    = definition+ !. { sequence $1 }

-- TODO: Enable line-based definitions.
definition :: Unification Type (String, HExpr Type)
    = (variable rhs ";") { liftTuple $1 }

rhs :: UnifiedHExpr Type
    = variable* "=" expr { foldr (\x y -> y >>= H.lambda x) $2 $1 }

expr :: UnifiedHExpr Type
    = cmpExpr

cmpExpr :: UnifiedHExpr Type
    = addExpr cmpOp addExpr { H.variable $2 |@@| $1 |@@| $3 }
    / addExpr

addExpr :: UnifiedHExpr Type
    = addExpr addOp mulExpr { H.variable $2 |@@| $1 |@@| $3 }
    / mulExpr

mulExpr :: UnifiedHExpr Type
    = mulExpr mulOp appExpr { H.variable $2 |@@| $1 |@@| $3 }
    / appExpr

appExpr :: UnifiedHExpr Type
    = simpleExpr+ { foldl1 (|@@|) $1 }

simpleExpr :: UnifiedHExpr Type
    = number { H.number $1 }
    / variable { H.variable $1 }
    / abstraction
    / "(" expr ")"

abstraction :: UnifiedHExpr Type
    = "\\" variable "." expr { $2 >>= H.lambda $1 }

number ::: Int
    = [0-9]+ { read $1 }

variable ::: String
    = [a-zA-Z_]+

cmpOp ::: String
    = "<=" { "le" }

-- multiplication not implemented yet
mulOp ::: String
    = !. { undefined }

addOp ::: String
    = "+" { "add" }
    / "-" { "sub" }

|]

liftTuple :: Monad m => (a, m b) -> m (a, b)
liftTuple (a, mb) = mb >>= \b -> return (a, b)

peggyParse :: String -> Either ParseError (Unification Type [(String, HExpr Type)])
peggyParse = parseString top "<stdin>"

showPos :: SrcPos -> String
showPos (SrcPos file _ line col) = file ++ ":" ++ show line ++ ":" ++ show col

showLoc :: SrcLoc -> String
showLoc (LocPos pos) = showPos pos
showLoc (LocSpan pos _) = showPos pos

showError :: ParseError -> String
showError (ParseError loc reason) = showLoc loc ++ ": error: " ++ reason
