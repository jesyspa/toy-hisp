module PeggyParser (
    peggyParse, showError
) where

import Text.Peggy
import Hisp

[peggy|

top :: [(String, HExpr)]
    = definition+ !.

-- TODO: Enable line-based definitions.
definition ::: (String, HExpr)
    = (variable "=" expr ";")

expr :: HExpr
    = subExpr+ { foldl1 (:@:) $1 }

subExpr ::: HExpr
    = number { Number $1 }
    / variable { Variable $1 }
    / abstraction
    / "(" expr ")"

number :: Int
    = [0-9]+ { read $1 }

variable :: String
    = [a-zA-Z_]+

abstraction :: HExpr
    = "\\" variable "." expr { lambda $1 $2 }

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
