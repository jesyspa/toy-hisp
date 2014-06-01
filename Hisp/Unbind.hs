module Hisp.Unbind (
    unbind,
    unbindTyped
) where

import Bound
import Control.Monad
import Data.Traversable
import Hisp.Hisp
import Hisp.SKI (Combinator(..), Comb(..), mkComb)
import Prelude hiding (any)

-- If the term has no bound variables, drop the unnecessary indirection and
-- return just that.  Otherwise, it's not that simple so return nothing.
--
-- I suspect that this is the wrong type for this due to the possibility of
-- subtrees which would mess up the types.  We'll have to see.  It's
-- correct for the unit typed case. :)
trivialize :: (Monad f, Traversable f) => Typed f () (Var () (f a)) -> Maybe (f a)
trivialize (Typed () e) = fmap join $ traverse f e
    where f (B _) = Nothing
          f (F x) = Just x


unbindScope :: (Functor (abs ()), Abstraction abs) => HispExpr Lambda () (Var () (HispExpr Lambda () a)) -> TypedHispExpr abs () (Comb a)
unbindScope (Variable (B ())) = mkComb I
unbindScope (Variable (F x)) = mkComb K |@| unbind x
unbindScope (Number x) = mkComb K |@| Typed () (Number x)
unbindScope (x :@: y) = case (trivialize x, trivialize y) of
                            (Nothing, Nothing) -> mkComb S |@| unbindTypedScope x |@| unbindTypedScope y
                            (Nothing, Just y') -> mkComb L |@| unbindTypedScope x |@| unbind y'
                            (Just x', Nothing) -> mkComb R |@| unbind x' |@| unbindTypedScope y
                            (Just x', Just y') -> mkComb K |@| (unbind x' |@| unbind y')
unbindScope (Abstraction (Lambda (Typed () s))) = doubleUnbindScope $ unscope s

doubleUnbindScope :: (Functor (abs ()), Abstraction abs) => HispExpr Lambda () (Var () (HispExpr Lambda () (Var () (HispExpr Lambda () a)))) -> TypedHispExpr abs () (Comb a)
doubleUnbindScope = joinComb . unbindScope . fmap flipCombVar . ignoreType . unbindScope

flipCombVar :: Comb (Var () (HispExpr Lambda () a)) -> Var () (HispExpr Lambda () (Comb a))
flipCombVar = fmap sequenceA . sequenceA

joinComb :: (Functor (abs ()), Abstraction abs) => TypedHispExpr abs () (Comb (Comb a)) -> TypedHispExpr abs () (Comb a)
joinComb = fmap (fmap join)

unbindTypedScope :: (Functor (abs ()), Abstraction abs) => TypedHispExpr Lambda () (Var () (HispExpr Lambda () a)) -> TypedHispExpr abs () (Comb a)
unbindTypedScope = unbindScope . ignoreType

unbind :: (Functor (abs ()), Abstraction abs) => HispExpr Lambda () a -> TypedHispExpr abs () (Comb a)
unbind (Variable x) = Typed () $ Variable $ Misc x
unbind (Number x) = Typed () $ Number x
unbind (x :@: y) = unbindTyped x |@| unbindTyped y
unbind (Abstraction (Lambda (Typed () scope))) = unbindScope $ unscope scope

unbindTyped :: (Functor (abs ()), Abstraction abs) => TypedHispExpr Lambda () a -> TypedHispExpr abs () (Comb a)
unbindTyped = unbind . ignoreType
