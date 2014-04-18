module SKI (
    SKI(..),
    Combinator(..),
    combName
)where

import Data.Foldable
import Data.Traversable

data Combinator = S | K | I | L | R deriving (Show, Read, Eq, Ord, Enum)

combName :: Combinator -> String
combName S = "comb_s"
combName K = "comb_k"
combName I = "comb_i"
combName L = "comb_l"
combName R = "comb_r"

infixl 6 :@:
data SKI a
    = Variable a
    | Number Int
    | Combinator Combinator
    | SKI a :@: SKI a
    deriving (Show, Read, Eq, Ord, Functor, Foldable, Traversable)

