module Unbind (
    unbind
) where

import Prelude hiding (any)
import Hisp
import SKI (Combinator(..), Comb(..))
import Bound
import Data.Maybe
import Data.Traversable
import Data.Foldable
import Control.Applicative
import Control.Monad

-- If the term has no bound variables, drop the unnecessary indirection and
-- return just that.  Otherwise, it's not that simple so return nothing.
trivialize :: (Monad f, Traversable f) => f (Var () (f a)) -> Maybe (f a)
trivialize = fmap join . traverse f
    where f (B _) = Nothing
          f (F x) = Just x


-- Used to turn Comb (g (h a)) into g (h (Comb a)).
flipCombVar :: (Traversable f, Functor f, Applicative g, Traversable g, Applicative h) => f (g (h a)) -> g (h (f a))
flipCombVar = fmap sequenceA . sequenceA


unbindScope :: (Functor abs, Abstraction abs) => HispExpr Lambda (Var () (HispExpr Lambda a)) -> HispExpr abs (Comb a)
unbindScope (Variable (B ())) = Variable (Comb I)
unbindScope (Variable (F x)) = Variable (Comb K) :@: unbind x
unbindScope (Number x) = Variable (Comb K) :@: Number x
unbindScope (x :@: y) = case (trivialize x, trivialize y) of
                            (Nothing, Nothing) -> Variable (Comb S) :@: unbindScope x :@: unbindScope y
                            (Nothing, Just y') -> Variable (Comb L) :@: unbindScope x :@: unbind y'
                            (Just x', Nothing) -> Variable (Comb R) :@: unbind x' :@: unbindScope y
                            (Just x', Just y') -> Variable (Comb K) :@: (unbind x' :@: unbind y')
unbindScope (Abstraction (Lambda s)) = fmap join . unbindScope . fmap flipCombVar . unbindScope $ unscope s

unbind :: (Functor abs, Abstraction abs) => HispExpr Lambda a -> HispExpr abs (Comb a)
unbind (Variable x) = Variable $ Misc x
unbind (Number x) = Number x
unbind (x :@: y) = unbind x :@: unbind y
unbind (Abstraction (Lambda scope)) = unbindScope $ unscope scope

