module Main where

import CodeGeneration
import Control.Applicative
import Control.Monad
import Data.ByteString.Lazy as BS
import Data.ByteString.Builder
import Hic
import Parser
import SkiToHic
import System.IO as IO
import Text.PrettyPrint.Leijen (pretty)
import Debug.Trace

printToFile :: Builder -> IO ()
printToFile = BS.writeFile "out.hic" . toLazyByteString

main :: IO ()
main = do
    contents <- IO.getContents
    let code = (skiToHic . compile) <$> parseHisp contents
    either (error.show) (printToFile.hicToByteString) code
