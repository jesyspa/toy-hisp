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
import Data.Traversable
import Hisp.Hisp

data Combinator = S | K | I | L | R deriving (Show, Read, Eq, Ord, Enum)

mkComb :: Combinator -> TypedHispExpr abs () (Comb a)
mkComb x = Typed () (Variable $ Comb x)

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
