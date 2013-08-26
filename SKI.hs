module SKI (
    SKI_Rec(..),
    SKI(..),
    Combinator(..),
    combName
)where

import Control.Applicative
import Control.Monad
import Control.Monad.Free
import Data.Foldable
import Data.Traversable
import Text.PrettyPrint.Leijen hiding ((<$>))

data Combinator = S | K | I | L | R deriving (Show, Read, Eq, Ord, Enum)

combName :: Combinator -> String
combName S = "comb_s"
combName K = "comb_k"
combName I = "comb_i"
combName L = "comb_l"
combName R = "comb_r"

infixl 6 :@:
data SKI_Rec a self
    = Variable a
    | Number Int
    | Combinator Combinator
    | self :@: self
    deriving (Show, Read, Eq, Ord, Functor, Traversable, Foldable)

type SKI a r = Free (SKI_Rec a) r

pprint :: Pretty a => SKI_Rec a (Bool -> Doc) -> Bool -> Doc
pprint (Variable a) _ = pretty a
pprint (Number x) _ = pretty x
pprint (Combinator x) _ = text $ show x
pprint (x :@: y) b | b = braces xy
                   | otherwise = xy
    where xy = x False <+> text ":@:" <+> y True

instance (Pretty a, Pretty r) => Pretty (SKI a r) where
    pretty x = iter pprint x' False
        where x' = fmap (const . pretty) x
