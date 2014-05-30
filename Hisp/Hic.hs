module Hisp.Hic (
    ObjectType(..),
    Hic(..),
    Field(..),
    Object(..),
    objectSize
) where

-- As a simplifying assumption, we make every object the maximum size (24
-- bytes).  This makes offset calculations much easier.
objectSize :: Int
objectSize = 24

data ObjectType = ApplicationType | NumberType | FunctionType | ForwarderType
                deriving (Show, Eq, Ord, Enum)

data Field a = Number Int
             | Function String
             | Ref a
             deriving (Eq, Ord, Show, Functor)

data Object a = Object ObjectType [Field a]
              deriving (Eq, Ord, Show, Functor)

data Hic = Hic { root :: Int, size :: Int, objects :: [Object Int]}
         deriving (Show, Eq, Ord)

