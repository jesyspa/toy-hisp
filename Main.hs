module Main where

import CodeGeneration
import Control.Applicative
import Parser
import SkiToHic
import System.IO
import Text.PrettyPrint.Leijen (pretty)

main :: IO ()
main = do
    contents <- getContents
    let code = (skiToHic . compile) <$> parseHisp contents
    either (error.show) print code
