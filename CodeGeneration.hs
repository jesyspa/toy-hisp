module CodeGeneration (
    compile
) where

import Prelude hiding (any)
import Hisp as H
import SKI as S
import Unbind

compile :: HispExpr Lambda a -> SKI a
compile = unbind

