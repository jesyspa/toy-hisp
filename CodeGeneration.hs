module CodeGeneration (
    compile
) where

import Prelude hiding (any)
import Hisp as H
import SKI as S
import Unbind

compile :: HispExpr a -> SKI a
compile = hispExprToSki . unbind

hispExprToSki :: HispExpr (Comb a) -> SKI a
hispExprToSki (Lambda _) = error "first run toSki over the expression"
hispExprToSki (H.Variable (Comb x)) = Combinator x
hispExprToSki (H.Variable (Misc x)) = S.Variable x
hispExprToSki (H.Number x) = S.Number x
hispExprToSki (x :@ y) = hispExprToSki x :@: hispExprToSki y

