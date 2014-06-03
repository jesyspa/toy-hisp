module Hisp.Hisp (
    Lambda(..),
    VoidAbs,
    HispExpr(..),
    HExpr,
    Abstraction,
    asAppliedTo,
    asAppliedToM,
    unifiedWith,
    lambda,
    variable,
    number,
    absurdAbs,
    Typed'(..),
    Typed,
    bindTyped,
    ignoreType,
    TypedHispExpr,
    UnifiedHExpr,
    TypedScope,
    (|@|),
    (|@@|)
) where

import Bound hiding (substitute)
import Control.Applicative
import Control.Monad
import Data.Foldable
import Data.Traversable
import Data.Void
import Prelude.Extras
import Hisp.TypeLike


class Abstraction abs where
    substitute :: abs ty a -> (a -> HispExpr abs ty c) -> abs ty c

data Typed' ty a = Typed ty a
                    deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

type Typed f ty a = Typed' ty (f a)

bindTyped :: (Monad f) => Typed f ty a -> (a -> f b) -> Typed f ty b
bindTyped x f = fmap (>>=f) x

unifiedWith :: (Monad (Unification ty), TypeLike ty) => Typed' ty a -> ty -> Unification ty (Typed' ty a)
unifiedWith (Typed tx x) t = liftM f $ unify tx t
    where f t' = Typed t' x

infixr 2 `asAppliedTo`
asAppliedTo :: (Monad (Unification ty), TypeLike ty) => Typed' ty a -> ty -> Unification ty (Typed' ty a)
asAppliedTo (Typed tx x) tp = do
        te <- fresh
        tf <- unapply tp te
        t' <- unify tf tx
        return $ Typed t' x

infixr 2 `asAppliedToM`
asAppliedToM :: (Monad (Unification ty), TypeLike ty) => Unification ty (Typed' ty a) -> ty -> Unification ty (Typed' ty a)
asAppliedToM x tp = x >>= \x' -> x' `asAppliedTo` tp


instance Applicative (Typed' ()) where
    pure = return
    (<*>) = ap

instance Monad (Typed' ()) where
    return = Typed ()
    Typed () x >>= f = f x

ignoreType :: Typed' ty a -> a
ignoreType (Typed _ x) = x

type TypedScope b f ty a = Typed (Scope b f) ty a


newtype Lambda ty a = Lambda { getLambda :: TypedScope () (HispExpr Lambda ty) ty a }
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


type TypedHispExpr abs ty a = Typed (HispExpr abs ty) ty a

infixl 5 :@:
data HispExpr abs ty a
    = Variable a
    | Number Int
    | TypedHispExpr abs ty a :@: TypedHispExpr abs ty a
    | Abstraction ty (abs ty a)
    deriving(Functor, Foldable, Traversable)

infixl 5 |@|
(|@|) :: (Monad (Unification ty), TypeLike ty) =>
    TypedHispExpr abs ty a -> TypedHispExpr abs ty a -> Unification ty (TypedHispExpr abs ty a)
x@(Typed tx _) |@| y@(Typed ty _) = do
        ta <- apply tx ty
        return $ Typed ta (x :@: y)

infixl 5 |@@|
(|@@|) :: (Monad (Unification ty), TypeLike ty) =>
    Unification ty (TypedHispExpr abs ty a) -> Unification ty (TypedHispExpr abs ty a) -> Unification ty (TypedHispExpr abs ty a)
x |@@| y = do
        x' <- x
        y' <- y
        x' |@| y'

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
    Abstraction tp x >>= f = Abstraction tp $ x `substitute` f

lambda :: (Monad (Unification ty), TypeLike ty, Eq a) => a -> TypedHispExpr Lambda ty a -> Unification ty (TypedHispExpr Lambda ty a)
lambda v (Typed te e) = do
        tp <- fresh
        tf <- unapply te tp
        let f = abstract1 v e
        return $ Typed tf $ Abstraction tp $ Lambda $ Typed te f

variable :: (Monad (Unification ty), TypeLike ty) => a -> Unification ty (TypedHispExpr abs ty a)
variable x = liftM f fresh
    where f tx = Typed tx (Variable x)

number :: (Monad (Unification ty), TypeLike ty) => Int -> Unification ty (TypedHispExpr abs ty a)
number x = liftM f $ constantType "Int"
    where f tx = Typed tx (Number x)

type HExpr ty = TypedHispExpr Lambda ty String
type UnifiedHExpr ty = Unification ty (HExpr ty)
