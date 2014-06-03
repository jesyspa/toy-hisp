module Hisp.SKI (
    HispExpr(..),
    TSKI,
    SKI,
    Combinator(..),
    Comb(..),
    combName,
    absurdAbs,
    mkComb
) where

import Control.Applicative
import Control.Monad
import Data.Foldable
import Data.Traversable hiding (sequence)
import Hisp.Hisp
import Hisp.TypeLike

data Combinator = S | K | I | L | R deriving (Show, Read, Eq, Ord, Enum)

mkComb :: (TypeLike ty, Monad (Unification ty)) => Combinator -> Unification ty (TypedHispExpr abs ty (Comb a))
mkComb x = liftM f $ combType x
    where f tx = Typed tx (Variable $ Comb x)

combType :: (TypeLike ty, Monad (Unification ty)) => Combinator -> Unification ty ty
combType S = do
        -- S f g x = f x (g x)
        -- (a -> b -> c) -> (a -> b) -> a -> c
        [a, b, c] <- sequence [fresh, fresh, fresh]
        tf <- unapplyMany c [b, a]
        tg <- unapply b a 
        unapplyMany c [a, tg, tf]
combType K = do
        -- K x y = x
        -- a -> b -> a
        [a, b] <- sequence [fresh, fresh]
        unapplyMany a [b, a]
combType I = do
        -- I x = x
        -- a -> a
        a <- fresh
        unapply a a
combType L = do
        -- L f x y = f y x
        -- (a -> b -> c) -> b -> a -> c
        [a, b, c] <- sequence [fresh, fresh, fresh]
        tf <- unapplyMany c [b, a]
        unapplyMany c [a, b, tf]
combType R = do
        -- R f g x = f (g x)
        -- (b -> c) -> (a -> b) -> a -> c
        [a, b, c] <- sequence [fresh, fresh, fresh]
        tf <- unapply c b
        tg <- unapply b a
        unapplyMany c [a, tg, tf]


data Comb a = Comb Combinator | Misc a deriving (Show, Read, Eq, Ord, Functor, Foldable, Traversable)

instance Applicative Comb where
    pure = return
    (<*>) = ap

instance Monad Comb where
    return = Misc
    Misc a >>= f = f a
    Comb x >>= _ = Comb x

combName :: Combinator -> String
combName S = "comb_s"
combName K = "comb_k"
combName I = "comb_i"
combName L = "comb_l"
combName R = "comb_r"

type SKI a = HispExpr VoidAbs () (Comb a)
type TSKI a = TypedHispExpr VoidAbs () (Comb a)
