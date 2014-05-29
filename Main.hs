module Main where

import CodeGeneration
import Control.Applicative
import Control.Monad
import Data.ByteString.Builder (toLazyByteString, Builder)
import Data.ByteString.Lazy (writeFile)
import Hic
import HicBuilder (hic)
import PeggyParser
import Prelude hiding (writeFile)
import SkiToHic
import System.IO (getContents, putStrLn)
import Text.PrettyPrint.Leijen (pretty)

printToFile :: Builder -> IO ()
printToFile = writeFile "out.hic" . toLazyByteString

main :: IO ()
main = do
    contents <- getContents
    let code = (skiToHic . compile) <$> peggyParse contents
    either (putStrLn.showError) (printToFile.hic) code
