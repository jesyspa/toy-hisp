module Main where

import Parser
import CodeGeneration
import SKI
import EvaluateSKI
import Visualisation
import Control.Applicative

c = flip evaluate (Name "arg") . compile

main = fmap c <$> (parseHisp <$> getContents) >>= print
