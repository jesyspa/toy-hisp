module Hisp (
    HispExpr(..),
    lambda
) where

import Bound
import Control.Applicative
import Control.Monad
import Data.Foldable
import Data.Traversable
import Prelude.Extras

infixl 6 :@
data HispExpr a
    = Variable a
    | Number Int
    | HispExpr a :@ HispExpr a
    | Lambda (Scope () HispExpr a)
    deriving(Show, Read, Eq, Ord, Functor, Foldable, Traversable)

instance Show1 HispExpr
instance Read1 HispExpr
instance Eq1 HispExpr
instance Ord1 HispExpr

instance Applicative HispExpr where
    pure = return
    (<*>) = ap

instance Monad HispExpr where
    return = Variable
    Variable a >>= f = f a
    Number x >>= _ = Number x
    (x :@ y) >>= f = (x >>= f) :@ (y >>= f)
    Lambda x >>= f = Lambda $ x >>>= f

lambda :: Eq a => a -> HispExpr a -> HispExpr a
lambda v = Lambda . abstract1 v




