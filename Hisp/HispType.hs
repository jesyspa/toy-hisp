module Hisp.HispType (
    HispType(..)
) where

import Hisp.TypeLike
import Control.Monad.Trans.State
import Control.Monad.IO.Class

data HispType = Free Int
              | Constant String
              | Arrow HispType HispType
              deriving (Eq, Ord, Read, Show)

instance TypeLike HispType where
    type Unification HispType = StateT Int IO
    fresh = get >>= \x -> put (x+1) >> return (Free x)
    unify x y = (liftIO $ putStrLn $ "infer: " ++ show x ++ " == " ++ show y) >> return x
    apply x y = case x of
                    Arrow _ b -> return b
                    _ -> do
                        te <- fresh
                        tf <- unapply te y
                        unify tf x
    unapply x y = return $ Arrow y x
    constantType s = return $ Constant s

