module SkiToHic (
    skiToHic
) where

import Hic
import SKI
import Control.Monad.Free
import Data.Foldable as F
import qualified Data.Set as S
import qualified Data.Map as M

subtrees :: Ord a => SKI a -> S.Set (SKI a)
subtrees x@(a :@: b) = x `S.insert`subtrees a `S.union` subtrees b
subtrees x = S.singleton x

number :: Ord a => Foldable f => f a -> M.Map a Int
number = snd . F.foldr f (0, M.empty)
    where f x (i, m) = (i+1, M.insert x i m)

toObject :: M.Map (SKI String) Int -> SKI String -> Object
toObject _ (Variable x) = Object FunctionType [Function x]
toObject _ (SKI.Number x) = Object NumberType [Hic.Number x]
toObject _ (Combinator c) = Object FunctionType [Function $ combName c]
toObject m (lhs :@: rhs) = Object ApplicationType [addr lhs, addr rhs]
    where addr = Pointer  . (objectSize*) . (m M.!)

makeObjects :: M.Map (SKI String) Int -> M.Map Int Object
makeObjects m = M.foldrWithKey f M.empty m
    where f k i = M.insert i (toObject m k)

-- We assume the keys are consecutive from 0 to size m
serialize :: M.Map Int Object -> [Object]
serialize = M.elems

skiToHic :: SKI String -> Hic
skiToHic ski = Hic (objectSize * objectOffsets M.! ski) (objectSize * length objects) objects
    where objectOffsets = number $ subtrees ski
          objects = serialize $ makeObjects objectOffsets
