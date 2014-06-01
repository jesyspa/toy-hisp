module Hisp.Hisp (
    Lambda(..),
    VoidAbs,
    HispExpr(..),
    HExpr,
    Abstraction,
    lambda,
    absurdAbs,
    Typed(..),
    liftTyped,
    bindTyped,
    ignoreType,
    TypedHispExpr,
    TypedScope,
    (|@|)
) where

import Bound hiding (substitute)
import Control.Applicative
import Control.Monad
import Data.Foldable
import Data.Traversable
import Data.Void
import Data.Monoid
import Prelude.Extras


class Abstraction abs where
    substitute :: abs ty a -> (a -> HispExpr abs ty c) -> abs ty c

data Typed f ty a = Typed ty (f a)
                  deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

liftTyped :: (f a -> g b) -> Typed f ty a -> Typed g ty b
liftTyped f (Typed tx x) = Typed tx (f x)

bindTyped :: (Monad f) => Typed f ty a -> (a -> f b) -> Typed f ty b
bindTyped x f = liftTyped (>>=f) x

ignoreType :: Typed f () a -> f a
ignoreType (Typed () x) = x

instance (Eq   ty, Eq1   f) => Eq1   (Typed f ty) where
    Typed tx x ==# Typed ty y = tx == ty && x ==# y
instance (Ord  ty, Ord1  f) => Ord1  (Typed f ty) where
    Typed tx x `compare1` Typed ty y = compare tx ty `mappend` compare1 x y
instance (Read ty, Read1 f) => Read1 (Typed f ty) where
    readsPrec1 = undefined
instance (Show ty, Show1 f) => Show1 (Typed f ty) where
    showsPrec1 = undefined

type TypedScope b f ty = Typed (Scope b f) ty


newtype Lambda ty name = Lambda { getLambda :: TypedScope () (HispExpr Lambda ty) ty name }
                    deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

instance Eq   ty => Eq1   (Lambda ty)
instance Ord  ty => Ord1  (Lambda ty)
instance Read ty => Read1 (Lambda ty)
instance Show ty => Show1 (Lambda ty)

instance Abstraction Lambda where
    substitute (Lambda (Typed tx x)) f = Lambda $ (Typed tx $ x >>>= f)


newtype VoidAbs ty name = VoidAbs { getVoidAbs :: Void }
                        deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

instance Eq1   (VoidAbs ty)
instance Ord1  (VoidAbs ty)
instance Read1 (VoidAbs ty)
instance Show1 (VoidAbs ty)

absurdAbs :: VoidAbs ty a -> b
absurdAbs = absurd . getVoidAbs

instance Abstraction VoidAbs where
    substitute (VoidAbs x) _ = VoidAbs x


type TypedHispExpr abs ty = Typed (HispExpr abs ty) ty

infixl 3 :@:
data HispExpr abs ty a
    = Variable a
    | Number Int
    | TypedHispExpr abs ty a :@: TypedHispExpr abs ty a
    | Abstraction (abs ty a)
    deriving(Functor, Foldable, Traversable)

infixl 3 |@|
(|@|) :: TypedHispExpr abs () a -> TypedHispExpr abs () a -> TypedHispExpr abs () a
x |@| y = Typed () (x :@: y)

instance Eq   ty => Eq1   (HispExpr Lambda ty)
instance Ord  ty => Ord1  (HispExpr Lambda ty)
instance Read ty => Read1 (HispExpr Lambda ty)
instance Show ty => Show1 (HispExpr Lambda ty)
instance Eq   ty => Eq1   (HispExpr VoidAbs ty)
instance Ord  ty => Ord1  (HispExpr VoidAbs ty)
instance Read ty => Read1 (HispExpr VoidAbs ty)
instance Show ty => Show1 (HispExpr VoidAbs ty)
deriving instance (Eq   ty, Eq   a) => Eq   (HispExpr Lambda ty a)
deriving instance (Ord  ty, Ord  a) => Ord  (HispExpr Lambda ty a)
deriving instance (Read ty, Read a) => Read (HispExpr Lambda ty a)
deriving instance (Show ty, Show a) => Show (HispExpr Lambda ty a)
deriving instance (Eq   ty, Eq   a) => Eq   (HispExpr VoidAbs ty a)
deriving instance (Ord  ty, Ord  a) => Ord  (HispExpr VoidAbs ty a)
deriving instance (Read ty, Read a) => Read (HispExpr VoidAbs ty a)
deriving instance (Show ty, Show a) => Show (HispExpr VoidAbs ty a)


instance (Abstraction abs, Functor (abs ty)) => Applicative (HispExpr abs ty) where
    pure = return
    (<*>) = ap

instance (Abstraction abs, Functor (abs ty)) => Monad (HispExpr abs ty) where
    return = Variable
    Variable a >>= f = f a
    Number x >>= _ = Number x
    (x :@: y) >>= f = x `bindTyped` f :@: y `bindTyped` f
    Abstraction x >>= f = Abstraction $ x `substitute` f

lambda :: Eq a => a -> HispExpr Lambda () a -> HispExpr Lambda () a
lambda v = Abstraction . Lambda . Typed () . abstract1 v

type HExpr = TypedHispExpr Lambda () String
