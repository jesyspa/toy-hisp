module Main where

import Control.Applicative
import Data.ByteString.Builder (toLazyByteString, Builder)
import Data.ByteString.Lazy (writeFile)
import Hisp.CodeGeneration
import Hisp.HicBuilder (hic)
import Hisp.PeggyParser
import Hisp.SkiToHic
import Control.Monad (liftM)
import Prelude hiding (writeFile)
import qualified Data.Map as M

printToFile :: Builder -> IO ()
printToFile = writeFile "out.hic" . toLazyByteString

main :: IO ()
main = do
    contents <- getContents
    let code = (skiToHic . compile . liftM M.fromList) <$> peggyParse contents
    either (putStrLn.showError) (printToFile.hic) code
