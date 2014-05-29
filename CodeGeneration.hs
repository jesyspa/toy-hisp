module CodeGeneration (
    compile
) where

import Hisp
import SKI
import Unbind
import Control.Arrow (second)

compile :: [(a, HispExpr Lambda a)] -> [(a, SKI a)]
compile = map (second unbind)

