module Hisp (
    Lambda(..),
    VoidAbs,
    HispExpr(..),
    Abstraction,
    lambda,
    absurdAbs
) where

import Bound hiding (substitute)
import Control.Applicative
import Control.Monad
import Data.Foldable
import Data.Traversable
import Data.Void
import Prelude.Extras


class Abstraction t where
    substitute :: t a -> (a -> HispExpr t c) -> t c


newtype Lambda name = Lambda { getLambda :: Scope () (HispExpr Lambda) name }
                    deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

instance Eq1   Lambda
instance Ord1  Lambda
instance Read1 Lambda
instance Show1 Lambda

instance Abstraction Lambda where
    substitute (Lambda x) f = Lambda $ x >>>= f

newtype VoidAbs name = VoidAbs { getVoidAbs :: Void }
                     deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

instance Eq1   VoidAbs
instance Ord1  VoidAbs
instance Read1 VoidAbs
instance Show1 VoidAbs

absurdAbs :: VoidAbs a -> b
absurdAbs = absurd . getVoidAbs

instance Abstraction VoidAbs where
    substitute (VoidAbs x) _ = VoidAbs x

infixl 3 :@:
data HispExpr abs a
    = Variable a
    | Number Int
    | HispExpr abs a :@: HispExpr abs a
    | Abstraction (abs a)
    deriving(Functor, Foldable, Traversable)

instance Eq1   (HispExpr Lambda)
instance Ord1  (HispExpr Lambda)
instance Read1 (HispExpr Lambda)
instance Show1 (HispExpr Lambda)
instance Eq1   (HispExpr VoidAbs)
instance Ord1  (HispExpr VoidAbs)
instance Read1 (HispExpr VoidAbs)
instance Show1 (HispExpr VoidAbs)
deriving instance Eq   a => Eq   (HispExpr Lambda a)
deriving instance Ord  a => Ord  (HispExpr Lambda a)
deriving instance Read a => Read (HispExpr Lambda a)
deriving instance Show a => Show (HispExpr Lambda a)
deriving instance Eq   a => Eq   (HispExpr VoidAbs a)
deriving instance Ord  a => Ord  (HispExpr VoidAbs a)
deriving instance Read a => Read (HispExpr VoidAbs a)
deriving instance Show a => Show (HispExpr VoidAbs a)


instance (Abstraction abs, Functor abs) => Applicative (HispExpr abs) where
    pure = return
    (<*>) = ap

instance Abstraction abs => Monad (HispExpr abs) where
    return = Variable
    Variable a >>= f = f a
    Number x >>= _ = Number x
    (x :@: y) >>= f = (x >>= f) :@: (y >>= f)
    Abstraction x >>= f = Abstraction $ x `substitute` f

lambda :: Eq a => a -> HispExpr Lambda a -> HispExpr Lambda a
lambda v = Abstraction . Lambda . abstract1 v




