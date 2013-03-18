module Main where

import Parser
import CodeGeneration
import Visualisation
import Control.Applicative

c = compile

main = fmap c <$> (parseHisp <$> getContents) >>= print
