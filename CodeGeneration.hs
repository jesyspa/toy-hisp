module CodeGeneration (
    compile
) where

import Prelude hiding (any)
import Hisp as H
import SKI as S
import Bound
import Data.Traversable
import Data.Foldable
import Control.Applicative
import Control.Monad
import Control.Monad.Free
import Unbind

compile :: HispExpr a -> SKI a String
compile = unfoldFree hispExprToSki . unbind

unfoldFree :: Functor f => (a -> Either r (f a)) -> a -> Free f r
unfoldFree f x = case f x of
    Left r -> return r
    Right v -> wrap $ fmap (unfoldFree f) v

hispExprToSki :: HispExpr (Comb a) -> Either String (SKI_Rec a (HispExpr (Comb a)))
hispExprToSki (Lambda x) = Left "first run toSki over the expression"
hispExprToSki (H.Variable (Comb x)) = Right $ Combinator x
hispExprToSki (H.Variable (Misc x)) = Right $ S.Variable x
hispExprToSki (H.Number x) = Right $ S.Number x
hispExprToSki (x :@ y) = Right $ x :@: y

