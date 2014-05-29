module Main where

import CodeGeneration
import Control.Applicative
import Data.ByteString.Builder (toLazyByteString, Builder)
import Data.ByteString.Lazy (writeFile)
import qualified Data.Map as M
import HicBuilder (hic)
import PeggyParser
import Prelude hiding (writeFile)
import SkiToHic

printToFile :: Builder -> IO ()
printToFile = writeFile "out.hic" . toLazyByteString

main :: IO ()
main = do
    contents <- getContents
    let code = (skiToHic . compile . M.fromList) <$> peggyParse contents
    either (putStrLn.showError) (printToFile.hic) code
