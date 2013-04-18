module Main where

import Text.Parsec
import CodeGeneration
import Control.Applicative
import Control.Arrow
import Control.Monad.Trans
import Parser
import SKI
import Visualisation
import ToCpp
import PrintCpp

main = do
    contents <- getContents
    let code = ski2cpp . compile <$> parseHisp contents
    either (error.show) print code
