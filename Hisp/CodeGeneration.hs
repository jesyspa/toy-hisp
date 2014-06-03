module Hisp.CodeGeneration (
    compile
) where

import Hisp.Hisp
import Hisp.SKI
import Hisp.Unbind
import Hisp.TypeLike
import qualified Data.Map as M
import Data.Traversable
import Prelude hiding (mapM)
import Hisp.HispType
import Control.Monad.State (evalStateT)

compile :: Ord a => Unification HispType (M.Map a (TypedHispExpr Lambda HispType a)) -> IO (M.Map a (SKI a))
compile m = flip evalStateT 0 $ fmap (M.map ignoreType) $ m >>= mapM unbindTyped


