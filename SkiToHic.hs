module SkiToHic (
    skiToHic
) where

import Data.Maybe
import Hic
import SKI
import Data.Foldable as F
import qualified Data.Set as S
import qualified Data.Map as M

subtrees :: Ord a => SKI a -> S.Set (SKI a)
subtrees x@(a :@: b) = x `S.insert`subtrees a `S.union` subtrees b
subtrees x = S.singleton x

toObject :: M.Map String (SKI String) -> SKI String -> Object (SKI String)
toObject globs (Variable (Misc x)) = case M.lookup x globs of
                                           Just addr -> Object ForwarderType [Ref addr]
                                           Nothing -> Object FunctionType [Function x]
toObject _     (Variable (Comb x)) = Object FunctionType [Function $ combName x]
toObject _     (SKI.Number x) = Object NumberType [Hic.Number x]
toObject _     (lhs :@: rhs) = Object ApplicationType [Ref lhs, Ref rhs]
toObject _     (Abstraction x) = absurdAbs x


number :: Ord a => S.Set a -> a -> Int
number = flip S.findIndex

makeObjects :: M.Map String (SKI String) -> S.Set (SKI String) -> M.Map (SKI String) (Object (SKI String))
makeObjects globs = M.fromSet (toObject globs)

skiToHic :: M.Map String (SKI String) -> Hic
skiToHic globals = Hic (objectSize * rootOffset) (objectSize * length numberedObjects) numberedObjects
    where trees =  S.unions $ map subtrees $ M.elems globals
          treeIndex = number trees
          objects = makeObjects globals trees
          numberedObjects = fmap (fmap ((objectSize*) . treeIndex)) $ M.elems objects
          rootTree = error "no main defined" `fromMaybe` M.lookup "main" globals
          rootOffset = treeIndex rootTree
