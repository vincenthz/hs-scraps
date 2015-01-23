-- |
-- Module      : Data.LookupTable
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : portable
--
-- A very fast and memory contiguous read-only lookup table
-- with an underlaying hashtable structure.
--
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
module Data.LookupTable
    ( LookupTable
    , lookupTable
    , lookupTableFind
    -- * statistical / debugging capability
    , LookupTableStat(..)
    , lookupTableStat
    , lookupTableDumpIndexes
    , lookupTableDumpData
    -- * Hash
    , hashFNV1
    , hashFNV1a
    ) where

import           Data.Bits (xor, unsafeShiftR)
import           Data.List (sortBy, group)
import           Data.Word
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import           GHC.Prim
import           GHC.Types
import           GHC.Exts
import           GHC.Ptr
import           GHC.Word
import           GHC.IO

import           Foreign.Ptr
import           Foreign.ForeignPtr
import           Foreign.Storable
import           Debug.Trace

type Key = ByteString

-- | A simple hash-indexed immutable array
data LookupTable a = LookupTable
    { hashFct   :: Key -> HashValue
    , hashIndex :: HashValue -> HashIndex
    , indexes   :: !Fanout
    , keys      :: !HashedKeys
    , values    :: !(ArrayTable a)
    }

data HashedKeys = HashedKeys ByteArray#

-- The array of a that are ordered in the same way of their hashValue
--
-- Optionally when collision are known to happens we
-- have the ability to store the key as part of this
data ArrayTable a =
      ArrayTableWithKey (Array# (Key, a))
    | ArrayTable        (Array# a)

-- Simple HashIndexes table
data Fanout = Fanout {-# UNPACK #-} !Int !ByteArray#

-- | a HashIndex, which is a transformation from a hash value to a smaller value
-- that give the ability to fit in the fanout table
type HashIndex = Int

{- FNV1 values
 - offset32    = 0x811c9dc5
 - offset64    = 0xcbf29ce484222325
 - fnv_prime32 = 0x01000193
 - fnv_prime64 = 0x100000001b3
 -}

-- | a HashValue
type HashValue = Word32

writeHashValueArray :: MutableByteArray# s -> Int# -> HashValue -> State# s -> State# s
writeHashValueArray ma n (W32# x) s = writeWord32Array# ma n x s
{-# INLINE writeHashValueArray #-}
indexHashValueArray :: ByteArray# -> Int# -> HashValue
indexHashValueArray ma n = W32# (indexWord32Array# ma n)
{-# INLINE indexHashValueArray #-}

type IndexValue = Word16

hashFNV1 :: Key -> HashValue
hashFNV1 = B.foldl' doAcc 0x811c9dc5
  where doAcc !hashVal !v = (hashVal * 0x01000193) `xor` fromIntegral v

hashFNV1a :: Key -> HashValue
hashFNV1a k = {-# SCC "hashFNV1a" #-} W32# (narrow32Word# (unW32 (B.foldl' doAcc 0x811c9dc5 k)))
  where doAcc !(W32# hashVal) !(W8# v) = W32# (0x01000193## `timesWord#` (hashVal `xor#` v))
        {-# INLINE doAcc #-}
        unW32 (W32# w) = w
        {-# INLINE unW32 #-}

{- 2 alternatives implementations:

* one the pure naive haskell version, which should be as fast as the one using core above,
  but turns out quite slow.

* a 64 bits access version, which give a little improvement on recent x86-64 cpu (even with unaligned access)
  but probably slower for all other architecture.
 
hashFNV1a :: Key -> HashValue
hashFNV1a = B.foldl' doAcc 0x811c9dc5
  where doAcc !hashVal !v = 0x01000193 * (hashVal `xor` fromIntegral v)

hashFNV1a :: Key -> HashValue
hashFNV1a k = unsafeDupablePerformIO $ withForeignPtr fptr $ \ptr ->
                loop 0x811c9dc5## (ptr `plusPtr` ofs) len
  where (B.PS fptr ofs len) = k
        loop !acc !ptr 0 = return $ W32# (narrow32Word# acc)
        loop !acc !ptr i
            | i > 8 = do
                    (W64# v) <- peek (castPtr ptr)
                    let v1 = v `and#` 0xff##
                        v2 = (GHC.Prim.uncheckedShiftRL# v 8#) `and#` 0xff##
                        v3 = (GHC.Prim.uncheckedShiftRL# v 16#) `and#` 0xff##
                        v4 = (GHC.Prim.uncheckedShiftRL# v 24#) `and#` 0xff##
                        v5 = (GHC.Prim.uncheckedShiftRL# v 32#) `and#` 0xff##
                        v6 = (GHC.Prim.uncheckedShiftRL# v 40#) `and#` 0xff##
                        v7 = (GHC.Prim.uncheckedShiftRL# v 48#) `and#` 0xff##
                        v8 = (GHC.Prim.uncheckedShiftRL# v 56#)
                    let acc1 = 0x01000193## `timesWord#` (acc `xor#` v1)
                        acc2 = 0x01000193## `timesWord#` (acc1 `xor#` v2)
                        acc3 = 0x01000193## `timesWord#` (acc2 `xor#` v3)
                        acc4 = 0x01000193## `timesWord#` (acc3 `xor#` v4)
                        acc5 = 0x01000193## `timesWord#` (acc4 `xor#` v5)
                        acc6 = 0x01000193## `timesWord#` (acc5 `xor#` v6)
                        acc7 = 0x01000193## `timesWord#` (acc6 `xor#` v7)
                        acc8 = 0x01000193## `timesWord#` (acc7 `xor#` v8)
                    loop acc8 (ptr `plusPtr` 8) (i-8)
            | otherwise = do
                    (W8# v) <- peek ptr
                    loop (0x01000193## `timesWord#` (acc `xor#` v)) (ptr `plusPtr` 1) (i-1)
        {-# INLINE loop #-}
-}

-- | Create a read only lookup table ByteString => a
--
-- The first parameter is used to size the fanout index table. This is up to the user to size
-- the fanout table according to needs and number of elements.
-- each bucket can contains up to 65536 elements.
--
-- 0 = 1 elements, 1 = 2 elements, 4 = 16 elements, 8 = 256 elements
-- 
lookupTable :: Int               -- ^ order of the fanout table (4 = 16 elements, 8 = 256 elements)
            -> [(ByteString, a)] -- ^ all the elements of the table in a key => value format
            -> LookupTable a
lookupTable indexOrder@(I# indexOrderI) elements
    | indexOrder < 0 || indexOrder > 16 = error "index order invalid. valid value is between 0 and 16"
    | nbElements == 0     = error "array index: no route defined"
    | nbElements > 0xffff = error "array index: too many elements"
    | otherwise           = {-# SCC "lookupTable" #-} unsafeDupablePerformIO $ do
        fanout  <- createFanout indexSize cats
        keysArr <- createPackedHash nbElements $ map fst sortedElements
        valsArr <- createValues nbElements $ map snd sortedElements
        return $ LookupTable { hashFct   = hHash
                             , hashIndex = computeIndex
                             , indexes   = fanout
                             , keys      = keysArr
                             , values    = valsArr
                             }
  where nbElements = length elements
        indexSize  = 2 ^ indexOrder
        hHash      = hashFNV1a

        cats  = complete 0 . group . map (computeIndex . fst) $ sortedElements

        computeIndex :: HashValue -> HashIndex
        computeIndex (W32# h) = I# (word2Int# (uncheckedShiftRL# h (32# -# indexOrderI)))

        sortedElements = sortBy (\r1 r2 -> fst r1 `compare` fst r2)
                       $ map (\ent@(r, _) -> (hHash r, ent)) elements

        -- make a list of arraySize elements where the non-existing category
        -- are populated with 0
        complete :: Int -> [[HashIndex]] -> [Int]
        complete i l
            | i == indexSize =
                case l of
                    [] -> []
                    _  -> error ("internal error: complete still have a list: " ++ show l)
            | otherwise =
                case l of
                    []              -> replicate (indexSize - i) 0
                    ([]:xs)         -> 0 : complete (i+1) xs
                    (x@(y:_):xs)
                        | i == y    -> length x : complete (i+1) xs
                        | otherwise -> 0 : complete (i+1) (x:xs)

        createPackedHash :: Int -> [HashValue] -> IO HashedKeys
        createPackedHash (I# nbElems) elems = IO $ \s1 ->
            let (# s2, ma #) = newByteArray# (nbElems *# 4#) s1
                s3           = fill ma 0# elems s2
                (# s4, a #)  = unsafeFreezeByteArray# ma s3
             in (# s4, HashedKeys a #)
          where fill _  _ []          s = s
                fill ma n (x:xs) s =
                    let s' = writeHashValueArray ma n x s
                     in fill ma (n +# 1#) xs s'

        createValues :: Int -> [(ByteString, a)] -> IO (ArrayTable a)
        createValues (I# nbElems) elems = IO $ \s1 ->
            let (# s2, ma #) = newArray# nbElems (head elems) s1
                s3           = fill ma 0# elems s2
                (# s4, a #)  = unsafeFreezeArray# ma s3
             in (# s4, ArrayTableWithKey a #)
          where fill _  _ []     s = s
                fill ma n (x:xs) s =
                    let s' = writeArray# ma n x s
                     in fill ma (n +# 1#) xs s'
{-# NOINLINE lookupTable #-}

-- create bucket indexes
createFanout :: Int -> [Int] -> IO Fanout
createFanout indexSize catSums = IO $ \s1 ->
    let (# s2 , ma #) = newPinnedByteArray# indexArraySize s1
        s3            = fill ma (accSums 0 catSums) s2
        (# s4, a #)   = unsafeFreezeByteArray# ma s3
     in (# s4, Fanout indexSize a #)
  where fill :: MutableByteArray# s -> [IndexValue] -> State# s -> State# s
        fill ma = loop 0#
          where loop _   []          s = s
                loop ofs (W16# w:xs) s =
                    loop (ofs +# 1#) xs (writeWord16Array# ma ofs w s)

        !(I# indexArraySize) = indexSize * 2

        accSums :: IndexValue -> [Int] -> [IndexValue]
        accSums i []     = [i]
        accSums i (x:xs) = let iAcc = i+fromIntegral x in iAcc : accSums iAcc xs

-- | Find the element associated with a bytestring
--
-- If the element is not found, then Nothing is returned.
--
-- TODO: when the min/max differential is big enough, move from linear to binary searching
lookupTableFind :: ByteString -> LookupTable a -> Maybe a
lookupTableFind searchedBs ha
    | b (diffIdx ==# 0#) = Nothing
    | b (diffIdx ==# 1#) = resolveValue (values ha) (findKey1 minIdx)
    | b (diffIdx <# 32#) = resolveValue (values ha) (findKeyLinear minIdx)
    | otherwise          = resolveValue (values ha) (findKeyBinary minIdx maxIdx)
  where -- find linearly in the hash array for the element; this is a fine strategy
        -- if the number of elements in this bucket (diffIdx is small),
        -- otherwise a binary search approch would yield faster capability
        findKeyLinear :: Int# -> Int#
        findKeyLinear i
            | b (i ==# maxIdx) = notFoundIndex
            | otherwise        = {-# SCC "findKeyLinear" #-}
                let h = indexHashValueArray keyArr i
                 in case h `compare` searchedHash of
                        EQ -> i
                        LT -> findKeyLinear (i +# 1#)
                        GT -> notFoundIndex
        findKeyBinary :: Int# -> Int# -> Int#
        findKeyBinary mi ma
            | b (diff <# 32#) = findKeyLinear mi
            | otherwise       = {-# SCC "findKeyBinary" #-}
                case h `compare` searchedHash of
                    EQ -> mid
                    LT -> findKeyBinary (mid +# 1#) ma
                    GT -> findKeyBinary mi mid 
          where !diff = ((ma -# mi) `quotInt#` 2#)
                !mid  = mi +# diff
                !h    = indexHashValueArray keyArr mid
                -- !mi = trace ("binary: " ++ show mi_ ++ " " ++ show ma) mi_
            
        -- there is only 1 element in this bucket so we specialize the lookup function
        findKey1 index = {-# SCC "findKey1" #-}
            let h = indexHashValueArray keyArr index
             in if h == searchedHash
                    then index
                    else notFoundIndex

        resolveValue :: ArrayTable a -> Int# -> Maybe a
        resolveValue at idx =
            if b (idx ==# notFoundIndex)
                then Nothing
                else case at of
                        ArrayTableWithKey a ->
                            let (# (bs, v) #) = indexArray# a idx
                             in if bs == searchedBs
                                    then Just v
                                    else resolveValue at (idx +# 1#)
                        ArrayTable a -> 
                            let (# v #) = indexArray# a idx in Just v

        !(HashedKeys keyArr)   = keys ha

        !searchedHash  = (hashFct ha) searchedBs
        !searchedIndex = (hashIndex ha) searchedHash

        (# minIdx, maxIdx #) = getFanout (indexes ha) searchedIndex
        !diffIdx             = maxIdx -# minIdx

        notFoundIndex = 0xffffffff#

        --b :: Int# -> Bool
        --b x = tagToEnum# x
        b = isTrue#
        -- with old GHC, isTrue doesn't exist, and id is enough. cannot use CPP because MagicHash ..

-- return the minimum and maximum index (excluded) where a hash of a specific category can be found.
-- if the minimum and maximum is the same value, then no elements of this category exists
--
-- no check is done if HashIndex is within bound of the fanout table
getFanout :: Fanout -> HashIndex -> (# Int#, Int# #)
getFanout (Fanout _ ba) iVal@(I# i)
    | iVal == 0 = (# 0#, (word2Int# (indexWord16Array# ba 0#)) #)
    | otherwise = (# (word2Int# (indexWord16Array# ba (i -# 1#)))
                  , (word2Int# (indexWord16Array# ba i)) #)

fanoutLength :: Fanout -> Int
fanoutLength (Fanout sz _) = sz

-- | Produce some statistics about the indexes used.
-- This is Useful for debugging / performance reasons
data LookupTableStat = LookupTableStat
    { hasBucketMin     :: Int
    , hasBucketMax     :: Int
    , hasBucketAverage :: Double
    } deriving (Show)

-- | Get some statistics about the use of the LookupTable
lookupTableStat :: LookupTable a -> LookupTableStat
lookupTableStat (LookupTable { indexes = fanout }) =
    LookupTableStat
        { hasBucketMin     = minimum bucketElements
        , hasBucketMax     = maximum bucketElements
        , hasBucketAverage = fromIntegral (sum bucketElements) / fromIntegral indexSize
        }
  where bucketElements = flip map [0..(indexSize-1)] $ \i ->
                let (# lowIndex, highIndex #) = getFanout fanout i
                 in I# highIndex - I# lowIndex
        indexSize = fanoutLength fanout

-- | Get all the indexes
lookupTableDumpIndexes :: LookupTable a -> [ (Int, (Int, Int)) ]
lookupTableDumpIndexes (LookupTable { indexes = fanout }) =
    flip map [0..(fanoutLength fanout)] $ \i -> (i, wrapFanoutData (getFanout fanout i))
  where wrapFanoutData (# i1, i2 #) = ( I# i1, I# i2 )

lookupTableDumpData :: LookupTable a -> [(HashIndex, HashValue, Maybe ByteString, a)]
lookupTableDumpData ha = dump (values ha) 0
  where (# _, !nbElemsI #) = getFanout (indexes ha) (fanoutLength (indexes ha) - 1)
        nbElems = I# nbElemsI
        dump t@(ArrayTableWithKey a) i@(I# index)
            | i == nbElems = []
            | otherwise     =
                let (# (b,r) #) = indexArray# a index
                    h           = indexHashValueArray keyArr index
                 in ((hashIndex ha) h, h, Just b, r) : dump t (i+1)
        dump t@(ArrayTable a) i@(I# index)
            | i == nbElems = []
            | otherwise    =
                let (# r #) = indexArray# a index
                    h       = indexHashValueArray keyArr index
                 in ((hashIndex ha) h, h, Nothing, r) : dump t (i+1)

        !(HashedKeys keyArr) = keys ha
