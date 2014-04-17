module Hic (
    Hic(..),
    Field(..),
    Object(..)
) where

import CodeGeneration

data Field = Number Int
           | Function String
           | Pointer Int
           deriving (Eq, Ord, Show)

data Object = Object Int [Field]
            deriving (Eq, Ord, Show)

data Hic = Hic { root :: Int, size :: Int, objects :: [Object]}
         deriving (Show, Eq, Ord)
