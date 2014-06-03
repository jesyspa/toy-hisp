module Hisp.SkiToHic (
    skiToHic
) where

import Control.Applicative
import Data.Maybe
import Hisp.Hisp
import Hisp.Hic as Hic
import Hisp.SKI as SKI
import qualified Data.Map as M
import qualified Data.Set as S

toOffset :: Int -> Int
toOffset = (objectSize*)

subtrees :: Ord a => SKI a -> S.Set (SKI a)
subtrees x@(a :@: b) = x `S.insert` typedSubtrees a `S.union` typedSubtrees b
subtrees x = S.singleton x

typedSubtrees :: Ord a => TSKI a -> S.Set (SKI a)
typedSubtrees = subtrees . ignoreType

toObject :: M.Map String (SKI String) -> SKI String -> Object (SKI String)
toObject globs (Variable (Misc x)) = case M.lookup x globs of
                                           Just addr -> Object ForwarderType [Ref addr]
                                           Nothing -> Object FunctionType [Function x]
toObject _     (Variable (Comb x)) = Object FunctionType [Function $ combName x]
toObject _     (SKI.Number x) = Object NumberType [Hic.Number x]
toObject _     (lhs :@: rhs) = Object ApplicationType [Ref $ ignoreType lhs, Ref $ ignoreType rhs]
toObject _     (Abstraction _ x) = absurdAbs x


makeObjects :: M.Map String (SKI String) -> S.Set (SKI String) -> M.Map (SKI String) (Object (SKI String))
makeObjects globs = M.fromSet (toObject globs)

skiToHic :: M.Map String (SKI String) -> Hic
skiToHic globals = Hic (numberTree rootTree) (objectSize * length numberedObjects) numberedObjects
    where trees =  S.unions $ map subtrees $ M.elems globals
          treeIndex x = S.findIndex x trees
          treeObjects = makeObjects globals trees
          numberTree = toOffset . treeIndex
          numberObject = fmap numberTree
          numberedObjects = numberObject <$> M.elems treeObjects
          rootTree = error "no main defined" `fromMaybe` M.lookup "main" globals
