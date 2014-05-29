module SKI (
    HispExpr(..),
    SKI,
    Combinator(..),
    Comb(..),
    combName,
    absurdAbs
) where

import Hisp
import Control.Applicative
import Control.Monad
import Data.Foldable
import Data.Traversable

data Combinator = S | K | I | L | R deriving (Show, Read, Eq, Ord, Enum)

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

type SKI a = HispExpr VoidAbs (Comb a)
