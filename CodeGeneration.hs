module CodeGeneration (
    compile
) where

import Hisp
import SKI
import Unbind
import qualified Data.Map as M

compile :: Ord a => M.Map a (HispExpr Lambda a) -> M.Map a (SKI a)
compile = M.map unbind

