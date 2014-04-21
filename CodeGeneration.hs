module CodeGeneration (
    compile
) where

import Hisp
import SKI
import Unbind

compile :: HispExpr Lambda a -> SKI a
compile = unbind

