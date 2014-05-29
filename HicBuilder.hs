module HicBuilder (
    hic
) where

import Data.ByteString.Builder
import Data.Monoid
import Hic

pad :: Int -> a -> [a] -> [a]
pad n v xs = xs ++ replicate (n - length xs) v

pad32, pad64 :: Builder
pad32 = word32LE 0
pad64 = word64LE 0

fixedString8 :: String -> Builder
fixedString8 = string8 . pad 8 '\0'

objectType :: ObjectType -> Builder
objectType ApplicationType = word32LE 0
objectType NumberType = word32LE 1
objectType FunctionType = word32LE 2
objectType ForwarderType = word32LE 3

field :: Field Int -> Builder
field (Number i) = word64LE $ fromIntegral i
field (Function str) = fixedString8 str
field (Ref ref) = word64LE $ fromIntegral ref

object :: Object Int -> Builder
object (Object tp fs) = mconcat $ [tpNum, objSize] ++ fields
    where tpNum = objectType tp
          objSize  = word32LE $ fromIntegral objectSize
          fields  = map field $ pad 2 (Number 0) fs

hic :: Hic -> Builder
hic (Hic r s objs) = mconcat $ [tag, pad32, pad64, rootOffset, heapSize] ++ body
    where tag = string8 "HISP"
          rootOffset = word64LE $ fromIntegral r
          heapSize = word64LE $ fromIntegral s
          body = map object objs
