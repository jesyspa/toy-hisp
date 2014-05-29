module SkiToHic (
    skiToHic
) where

import Control.Arrow (right)
import Debug.Trace
import Data.Maybe
import Hic
import SKI
import Data.Foldable as F
import qualified Data.Set as S
import qualified Data.Map as M

trace' x = trace (show x) x

toField :: Int -> Field
toField = Hic.Number . (objectSize*)

subtrees :: Ord a => SKI a -> S.Set (SKI a)
subtrees x@(a :@: b) = x `S.insert`subtrees a `S.union` subtrees b
subtrees x = S.singleton x

number :: Ord a => Foldable f => f a -> M.Map a Int
number = snd . F.foldr f (0, M.empty)
    where f x (i, m) = (i+1, M.insert x i m)

toObject :: M.Map String Int -> M.Map (SKI String) Int -> SKI String -> Object
toObject globs _ (Variable (Misc x)) = case M.lookup x globs of
                                           Just addr -> Object ForwarderType [toField addr]
                                           Nothing -> Object FunctionType [Function x]
toObject _ _     (Variable (Comb x)) = Object FunctionType [Function $ combName x]
toObject _ _     (SKI.Number x) = Object NumberType [Hic.Number x]
toObject _ objs  (lhs :@: rhs) = Object ApplicationType [toField $ objs M.! lhs, toField $ objs M.! rhs]
toObject _ _     (Abstraction x) = absurdAbs x

makeObjects :: M.Map String Int -> M.Map (SKI String) Int -> M.Map Int Object
makeObjects globs objs = M.foldrWithKey f M.empty objs
    where f k i = M.insert i (toObject globs objs k)

-- We assume the keys are consecutive from 0 to size m
serialize :: M.Map Int Object -> [Object]
serialize = M.elems

skiToHic :: [(String, SKI String)] -> Hic
skiToHic globalsAssoc = Hic (objectSize * rootOffset) (objectSize * length objects) objects
    where objOffsets = number $ S.unions $ map (subtrees.snd) globalsAssoc
          globals = M.fromList globalsAssoc
          globOffsets = M.map (objOffsets M.!) globals
          objects = serialize $ makeObjects globOffsets objOffsets
          rootOffset = error "no main defined" `fromMaybe` M.lookup "main" globOffsets
