module Hisp.CodeGeneration (
    compile
) where

import Hisp.Hisp
import Hisp.SKI
import Hisp.Unbind
import Hisp.TypeLike
import qualified Data.Map as M
import Control.Monad.Identity (runIdentity)
import Data.Traversable
import Prelude hiding (mapM)

compile :: Ord a => Unification () (M.Map a (TypedHispExpr Lambda () a)) -> M.Map a (SKI a)
compile m = M.map ignoreType $ runIdentity $ m >>= mapM unbindTyped


