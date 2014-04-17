module CodeGeneration (
    compile
) where

import Prelude hiding (any)
import Hisp as H
import SKI as S
import Control.Monad.Free
import Unbind

compile :: HispExpr a -> SKI a String
compile = unfoldFree hispExprToSki . unbind

-- What do you mean, Free doesn't have anamorphism support?
unfoldFree :: Functor f => (a -> Either r (f a)) -> a -> Free f r
unfoldFree f x = case f x of
    Left r -> return r
    Right v -> wrap $ fmap (unfoldFree f) v

hispExprToSki :: HispExpr (Comb a) -> Either String (SKIRec a (HispExpr (Comb a)))
hispExprToSki (Lambda _) = Left "first run toSki over the expression"
hispExprToSki (H.Variable (Comb x)) = Right $ Combinator x
hispExprToSki (H.Variable (Misc x)) = Right $ S.Variable x
hispExprToSki (H.Number x) = Right $ S.Number x
hispExprToSki (x :@ y) = Right $ x :@: y

