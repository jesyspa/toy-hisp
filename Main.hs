module Main where

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
    let ast = peggyParse contents
    case ast of
        Left err -> putStrLn $ showError err
        Right ast' -> do
            skiCode <- compile $ liftM M.fromList $ ast'
            print skiCode
            let code = skiToHic skiCode
            printToFile $ hic code

