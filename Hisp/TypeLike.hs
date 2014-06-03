module Hisp.TypeLike (
    TypeLike,
    Unification,
    fresh,
    unify,
    apply,
    unapply,
    constantType,
    unapplyMany
) where

import Control.Monad.Identity

class TypeLike ty where
    type Unification ty :: * -> *
    -- get a new type
    fresh :: Unification ty ty
    -- make two types equal and yield some representative
    unify :: ty -> ty -> Unification ty ty
    -- combine a -> b and a to get b
    apply :: ty -> ty -> Unification ty ty
    -- uncombine b and a to get a -> b
    unapply :: ty -> ty -> Unification ty ty
    -- make a specific type
    constantType :: String -> Unification ty ty

unapplyMany :: (TypeLike ty, Monad (Unification ty)) => ty -> [ty] -> Unification ty ty
unapplyMany t (x:xs) = unapply t x >>= \tn -> unapplyMany tn xs
unapplyMany t [] = return t

instance TypeLike () where
    type Unification () = Identity
    fresh = return ()
    unify _ _ = return ()
    apply _ _ = return ()
    unapply _ _ = return ()
    constantType _ = return ()

