{-# OPTIONS_GHC -fno-warn-unused-do-bind -fno-warn-unused-matches #-}
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
-- Peggy produces many spurrious warnings this way.
module Hisp.PeggyParser (
    peggyParse, showError
) where

import Hisp.Hisp
import Text.Peggy

[peggy|

top :: [(String, HExpr)]
    = definition+ !.

-- TODO: Enable line-based definitions.
definition :: (String, HExpr)
    = (variable rhs ";")

rhs :: HExpr
    = variable* "=" expr { foldr lambda $2 $1 }

expr :: HExpr
    = cmpExpr

cmpExpr :: HExpr
    = addExpr cmpOp addExpr { Variable $2 :@: $1 :@: $3 }
    / addExpr

addExpr :: HExpr
    = addExpr addOp mulExpr { Variable $2 :@: $1 :@: $3 }
    / mulExpr

mulExpr :: HExpr
    = mulExpr mulOp appExpr { Variable $2 :@: $1 :@: $3 }
    / appExpr

appExpr :: HExpr
    = simpleExpr+ { foldl1 (:@:) $1 }

simpleExpr :: HExpr
    = number { Number $1 }
    / variable { Variable $1 }
    / abstraction
    / "(" expr ")"

abstraction :: HExpr
    = "\\" variable "." expr { lambda $1 $2 }

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

peggyParse :: String -> Either ParseError [(String, HExpr)]
peggyParse = parseString top "<stdin>"

showPos :: SrcPos -> String
showPos (SrcPos file _ line col) = file ++ ":" ++ show line ++ ":" ++ show col

showLoc :: SrcLoc -> String
showLoc (LocPos pos) = showPos pos
showLoc (LocSpan pos _) = showPos pos

showError :: ParseError -> String
showError (ParseError loc reason) = showLoc loc ++ ": error: " ++ reason
