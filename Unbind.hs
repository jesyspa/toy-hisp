module Unbind (
    Comb(..),
    unbind
) where

import Prelude hiding (any)
import Hisp
import SKI (Combinator(..))
import Bound
import Data.Traversable
import Data.Foldable
import Control.Applicative
import Control.Monad

data Comb a = Comb Combinator | Misc a deriving (Show, Read, Eq, Ord, Functor, Foldable, Traversable)

instance Applicative Comb where
    pure = return
    (<*>) = ap

instance Monad Comb where
    return = Misc
    Misc a >>= f = f a
    Comb x >>= _ = Comb x

-- TODO: What does this do again?
trivialize :: (Monad f, Traversable f) => f (Var () (f a)) -> Maybe (f a)
trivialize = fmap join . traverse f
    where f (B _) = Nothing
          f (F x) = Just x

-- Or this, for that matter...
flipCombVar :: Comb (Var () (HispExpr a)) -> Var () (HispExpr (Comb a))
flipCombVar = fmap sequenceA . sequenceA


unbind :: HispExpr a -> HispExpr (Comb a)
unbind (Variable x) = Variable $ Misc x
unbind (Number x) = Number x
unbind (x :@ y) = unbind x :@ unbind y
unbind (Lambda scope) = go $ unscope scope
    where go :: HispExpr (Var () (HispExpr a)) -> HispExpr (Comb a)
          go (Variable (B ())) = Variable (Comb I)
          go (Variable (F x)) = Variable (Comb K) :@ fmap Misc x
          go (Number x) = Variable (Comb K) :@ Number x
          go (x :@ y) = case (trivialize x, trivialize y) of
                            (Nothing, Nothing) -> Variable (Comb S) :@ go x :@ go y
                            (Nothing, Just y') -> Variable (Comb L) :@ go x :@ unbind y'
                            (Just x', Nothing) -> Variable (Comb R) :@ unbind x' :@ go y
                            (Just x', Just y') -> Variable (Comb K) :@ (unbind x' :@ unbind y')
          go (Lambda s) = fmap join . go . fmap flipCombVar . go $ unscope s

