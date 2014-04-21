module Hic (
    ObjectType(..),
    Hic(..),
    Field(..),
    Object(..),
    objectSize
) where

import CodeGeneration

-- As a simplifying assumption, we make every object the maximum size (24
-- bytes).  This makes offset calculations much easier.
objectSize :: Int
objectSize = 32

data ObjectType = ApplicationType | NumberType | FunctionType
                deriving (Show, Eq, Ord, Enum)

data Field = Number Int
           | Function String
           deriving (Eq, Ord, Show)

data Object = Object ObjectType [Field]
            deriving (Eq, Ord, Show)

data Hic = Hic { root :: Int, size :: Int, objects :: [Object]}
         deriving (Show, Eq, Ord)

