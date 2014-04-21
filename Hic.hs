module Hic (
    ObjectType(..),
    Hic(..),
    Field(..),
    Object(..),
    objectSize,
    hicToByteString
) where

import Data.Monoid
import Data.ByteString.Builder
import Data.Int
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

pad :: Int -> a -> [a] -> [a]
pad n v xs = xs ++ replicate (n - length xs) v

pad32, pad64 :: Builder
pad32 = word32LE 0
pad64 = word64LE 0

build8ByteString :: String -> Builder
build8ByteString = string8 . pad 8 '\0'

objectTypeToByteString :: ObjectType -> Builder
objectTypeToByteString ApplicationType = word32LE 0
objectTypeToByteString NumberType = word32LE 1
objectTypeToByteString FunctionType = word32LE 2

fieldToByteString :: Field -> Builder
fieldToByteString (Number i) = word64LE $ fromIntegral i
fieldToByteString (Function str) = build8ByteString str

objectToByteString :: Object -> Builder
objectToByteString (Object tp fs) = mconcat $ [tpNum, objSize, pad64] ++ fields
    where tpNum = objectTypeToByteString tp
          objSize  = word32LE $ fromIntegral objectSize
          fields  = map fieldToByteString $ pad 2 (Number 0) fs

hicToByteString :: Hic -> Builder
hicToByteString (Hic r s objs) = mconcat $ [tag, pad32, pad64, rootOffset, heapSize] ++ body
    where tag = word32BE 0x48495350
          rootOffset = word64LE $ fromIntegral r
          heapSize = word64LE $ fromIntegral s
          body = map objectToByteString objs
