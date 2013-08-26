module Main where

import Text.Parsec
import Control.Applicative
import Control.Arrow
import Control.Monad.Trans
import Parser
import Cpp
import CodeGeneration
import Text.PrettyPrint.Leijen hiding ((<$>))

main = do
    contents <- getContents
    let code = compile <$> parseHisp contents
    either (error.show) (print.pretty) code
