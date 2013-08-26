module Main where

import Text.Parsec
import Control.Applicative
import Control.Arrow
import Control.Monad.Trans
import Parser
import CodeGeneration
import SkiToCpp
import SkiToC
import SKI
import Cpp
import Text.PrettyPrint.Leijen (pretty)
import System.Environment

chooseTarget :: [String] -> (SKI String r -> CppFile)
chooseTarget xs | null xs = skiToCpp
                | head xs == "-C" = skiToC
                | otherwise = skiToCpp

main = do
    contents <- getContents
    args <- getArgs
    let code = (chooseTarget args . compile) <$> parseHisp contents
    either (error.show) (print.pretty) code
