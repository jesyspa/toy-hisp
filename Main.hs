module Main where

import Control.Applicative
import Parser
import CodeGeneration
import SkiToCpp
import Text.PrettyPrint.Leijen (pretty)

main :: IO ()
main = do
    contents <- getContents
    let code = (skiToCpp . compile) <$> parseHisp contents
    either (error.show) (print.pretty) code
