module Hisp.Unbind (
    unbind,
    unbindTyped
) where

import Bound
import Control.Monad
import Data.Traversable
import Hisp.Hisp
import Hisp.TypeLike
import Hisp.SKI (Combinator(..), Comb(..), mkComb)
import Prelude hiding (any)

-- If the term has no bound variables, drop the unnecessary indirection and
-- return just that.  Otherwise, it's not that simple so return nothing.
--
-- I suspect that this is the wrong type for this due to the possibility of
-- subtrees which would mess up the types.  We'll have to see.  It's
-- correct for the unit typed case. :)
trivialize :: (Monad (Unification ty), TypeLike ty, Monad f, Traversable f) =>
    Typed f ty (Var () (f a)) -> Maybe (f a)
trivialize (Typed _ e) = fmap join $ traverse f e
    where f (B _) = Nothing
          f (F x) = Just x


unbindScope :: (Functor (abs ty), Monad (Unification ty), TypeLike ty, Abstraction abs) =>
    ty -> HispExpr Lambda ty (Var () (HispExpr Lambda ty a)) -> Unification ty (TypedHispExpr abs ty (Comb a))
unbindScope tp s = go s `asAppliedToM` tp
    where go (Variable (B ())) = mkComb I
          go (Variable (F x)) = mkComb K |@@| unbind x
          go (Number x) = mkComb K |@@| number x
          go (x :@: y) = case (trivialize x, trivialize y) of
                              (Nothing, Nothing) -> mkComb S |@@| unbindTypedScope tp x |@@| unbindTypedScope tp y
                              (Nothing, Just y') -> mkComb L |@@| unbindTypedScope tp x |@@| unbind y'
                              (Just x', Nothing) -> mkComb R |@@| unbind x' |@@| unbindTypedScope tp y
                              (Just x', Just y') -> mkComb K |@@| (unbind x' |@@| unbind y')
          go (Abstraction tp' (Lambda (Typed te e))) = doubleUnbindScope tp tp' te $ unscope e

doubleUnbindScope :: (Functor (abs ty), Abstraction abs, TypeLike ty, Monad (Unification ty)) =>
    ty -> ty -> ty -> HispExpr Lambda ty (Var () (HispExpr Lambda ty (Var () (HispExpr Lambda ty a))))
        -> Unification ty (TypedHispExpr abs ty (Comb a))
doubleUnbindScope tpo tpi te e = do
        Typed tci ci <- unbindScope tpi e
        tfi <- unapply te tpi
        tfi' <- unify tci tfi
        let ci' = fmap flipCombVar ci
        Typed tcio cio <- unbindScope tpo ci'
        tfio <- unapply tfi' tpo
        tfio' <- unify tcio tfio
        return $ joinComb (Typed tfio' cio)

flipCombVar :: Comb (Var () (HispExpr Lambda ty a)) -> Var () (HispExpr Lambda ty (Comb a))
flipCombVar = fmap sequenceA . sequenceA

joinComb :: (Functor (abs ty), Abstraction abs) => TypedHispExpr abs ty (Comb (Comb a)) -> TypedHispExpr abs ty (Comb a)
joinComb = fmap (fmap join)

unbindTypedScope :: (Functor (abs ty), TypeLike ty, Monad (Unification ty), Abstraction abs) =>
    ty -> TypedHispExpr Lambda ty (Var () (HispExpr Lambda ty a)) -> Unification ty (TypedHispExpr abs ty (Comb a))
unbindTypedScope tp (Typed te e) = do
        Typed tf f <- unbindScope tp e
        tpe <- unapply te tp
        tf' <- unify tpe tf
        return (Typed tf' f)

unbind :: (Functor (abs ty), TypeLike ty, Monad (Unification ty), Abstraction abs) =>
    HispExpr Lambda ty a -> Unification ty (TypedHispExpr abs ty (Comb a))
unbind (Variable x) = variable $ Misc x
unbind (Number x) = number x
unbind (x :@: y) = unbindTyped x |@@| unbindTyped y
unbind (Abstraction tp (Lambda (Typed te scope))) = do
        tf <- unapply te tp
        Typed tc c <- unbindScope tp $ unscope scope
        t <- unify tf tc
        return $ Typed t c

unbindTyped :: (Functor (abs ty), Abstraction abs, TypeLike ty, Monad (Unification ty)) =>
    TypedHispExpr Lambda ty a -> Unification ty (TypedHispExpr abs ty (Comb a))
unbindTyped (Typed te e) = do
        Typed te' e' <- unbind e
        t <- unify te te'
        return $ Typed t e'
