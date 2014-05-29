module SkiToHic (
    skiToHic
) where

import Control.Applicative
import Data.Maybe
import Hic
import SKI
import qualified Data.Set as S
import qualified Data.Map as M

toOffset :: Int -> Int
toOffset = (objectSize*)

subtrees :: Ord a => SKI a -> S.Set (SKI a)
subtrees x@(a :@: b) = x `S.insert` subtrees a `S.union` subtrees b
subtrees x = S.singleton x

toObject :: M.Map String (SKI String) -> SKI String -> Object (SKI String)
toObject globs (Variable (Misc x)) = case M.lookup x globs of
                                           Just addr -> Object ForwarderType [Ref addr]
                                           Nothing -> Object FunctionType [Function x]
toObject _     (Variable (Comb x)) = Object FunctionType [Function $ combName x]
toObject _     (SKI.Number x) = Object NumberType [Hic.Number x]
toObject _     (lhs :@: rhs) = Object ApplicationType [Ref lhs, Ref rhs]
toObject _     (Abstraction x) = absurdAbs x


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
