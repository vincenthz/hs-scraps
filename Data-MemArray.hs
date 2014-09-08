{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
-- |
-- Module      : Data.MemArray
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : portable
--
-- A simple array abstraction that allow to use typed
-- array of bytes where the array is pinned in memory
-- to allow easy use with Foreign interfaces, ByteString
-- and always aligned to 64 bytes.
--
-- Import this module qualified
--
module Data.MemArray
    ( Array
    , MutableArray
    , ArrayElem(..)
    -- * methods
    , length
    , mutableLength
    , copy
    -- * Creation
    , new
    , create
    , fromList
    , toList
    -- * accessors
    , index
    , read
    , write
    , update
    , unsafeIndex
    , unsafeRead
    , unsafeWrite
    , unsafeUpdate
    -- * Functions
    , append
    , concat
    -- * Mutable and Immutable
    , thaw
    , unsafeThaw
    , unsafeFreeze
    ) where

import GHC.Prim
import GHC.ST
import GHC.Word
import GHC.Types
import GHC.Exts (Ptr(..))
import qualified GHC.Exts as Ext
import Data.Byteable
import Data.Monoid
--import Prelude (($), otherwise, undefined, error, (<), (>), (||), Eq)
import Prelude hiding (length, concat, read)
import qualified Prelude
import Control.Monad.ST.Unsafe (unsafeIOToST)
import Control.Monad.ST (stToIO)
import System.IO.Unsafe (unsafePerformIO)

import Foreign.ForeignPtr (newForeignPtr_)
import qualified Data.ByteString.Internal as B

-- | An array of type built on top of GHC primitive.
--
-- The elements need to have fixed sized and the representation is a
-- packed contiguous array in memory that can easily be passed
-- to foreign interface
data Array ty = A !Int# ByteArray#

-- | A Mutable array of types built on top of GHC primitive.
--
-- Element in this array can be modified in place.
data MutableArray ty st = MA Int# (MutableByteArray# st)

instance (ArrayElem ty, Eq ty) => Eq (Array ty) where
    a == b
        | la /= lb  = False
        | otherwise = loop (la - 1)
      where loop n | n == 0    = indexElem a n == indexElem b n
                   | otherwise = (indexElem a n == indexElem b n) && loop (n-1)
            la = length a
            lb = length b

sizeInBitsOfContent :: ArrayElem ty => Array ty -> Int
sizeInBitsOfContent = getSize undefined
  where getSize :: ArrayElem ty => ty -> Array ty -> Int
        getSize ty _ = sizeInBits ty
{-# INLINE sizeInBitsOfContent #-}

instance ArrayElem ty => Byteable (Array ty) where
    toBytes ar = unsafePerformIO $ withConstPtr ar $ \ptr -> do
        fptr <- newForeignPtr_ ptr
        return $ B.PS fptr 0 nbBytes
      where nbBytes = byteableLength ar
    byteableLength a@(A len _) = I# (quotInt# (roundUp (len *# szBits) 8#) 8#)
      where !(I# szBits) = sizeInBitsOfContent a
            roundUp :: Int# -> Int# -> Int#
            roundUp i d = negateInt# (andI# (negateInt# i)  (negateInt# d))
    withBytePtr ar = withConstPtr ar

-- | Represent the accessor for types that can be stored in the Array and MutableArray.
--
-- Types need to be a instance of storable and have fixed sized.
class ArrayElem ty where
    -- | get the size in bits of a ty element
    sizeInBits :: ty -> Int

    -----
    -- Array section
    -----

    -- | return the element stored at a specific index
    indexElem :: Array ty -> Int -> ty

    -----
    -- Mutable Array section
    -----

    -- | Read an element at an index in a mutable array
    readElem :: MutableArray ty st -- ^ mutable array to read from
             -> Int                -- ^ index of the element to retrieve
             -> ST st ty           -- ^ the element returned

    -- | Write an element to a specific cell in a mutable array.
    writeElem :: MutableArray ty st -- ^ mutable array to modify
              -> Int                -- ^ index of the element to modify
              -> ty                 -- ^ the new value to store
              -> ST st ()

-- return the index and mask to a bit in a bitmap
bitmapAddr :: Int# -> (# Int# , Word# #)
bitmapAddr !i = (# idx, mask #)
  where (# !idx, !bit #) = quotRemInt# i 4#
        !mask = case bit of
                    0#  -> 0x1##
                    1#  -> 0x2##
                    2#  -> 0x4##
                    3#  -> 0x8##
                    4#  -> 0x10##
                    5#  -> 0x20##
                    6#  -> 0x40##
                    7#  -> 0x80##
                    8#  -> 0x100##
                    9#  -> 0x200##
                    10# -> 0x400##
                    11# -> 0x800##
                    12# -> 0x1000##
                    13# -> 0x2000##
                    14# -> 0x4000##
                    15# -> 0x8000##
                    16# -> 0x10000##
                    17# -> 0x20000##
                    18# -> 0x40000##
                    19# -> 0x80000##
                    20# -> 0x100000##
                    21# -> 0x200000##
                    22# -> 0x400000##
                    23# -> 0x800000##
                    24# -> 0x1000000##
                    25# -> 0x2000000##
                    26# -> 0x4000000##
                    27# -> 0x8000000##
                    28# -> 0x10000000##
                    29# -> 0x20000000##
                    30# -> 0x40000000##
                    _   -> 0x80000000##

instance ArrayElem Bool where
    sizeInBits _ = 1
    {-# INLINE sizeInBits #-}
    indexElem (A _ ba) (I# n) =
         isTrue# (0# /=# word2Int# (and# v mask))
      where (# idx, mask #) = bitmapAddr n
            !v = indexWord32Array# ba idx
    {-# INLINE indexElem #-}
    readElem (MA _ mba) (I# n) = ST $ \s1 ->
        case readWord32Array# mba idx s1 of
            (# s2, v #) -> (# s2, isTrue# (word2Int# (and# v mask) ==# 0#) #)
      where (# !idx, !mask #) = bitmapAddr n
    {-# INLINE readElem #-}
    writeElem (MA _ mba) (I# n) setValue = ST $ \s1 ->
        case readWord32Array# mba idx s1 of
            (# s2, v #) -> (# writeWord32Array# mba idx (newVal v) s2, () #)
      where (# !idx, !mask #) = bitmapAddr n
            newVal v
                | setValue  = or# v mask
                | otherwise = and# v (not# mask)
    {-# INLINE writeElem #-}
      
instance ArrayElem Word8 where
    sizeInBits _ = 8
    {-# INLINE sizeInBits #-}
    indexElem (A _ ba) (I# n) = W8# (indexWord8Array# ba n)
    {-# INLINE indexElem #-}
    readElem = readArrayST readWord8Array# W8#
    {-# INLINE readElem #-}
    writeElem mba n (W8# w) = writeArrayST writeWord8Array# w mba n
    {-# INLINE writeElem #-}
instance ArrayElem Word16 where
    sizeInBits _ = 16
    {-# INLINE sizeInBits #-}
    indexElem (A _ ba) (I# n) = W16# (indexWord16Array# ba n)
    {-# INLINE indexElem #-}
    readElem = readArrayST readWord16Array# W16#
    {-# INLINE readElem #-}
    writeElem mba n (W16# w) = writeArrayST writeWord16Array# w mba n
    {-# INLINE writeElem #-}
instance ArrayElem Word32 where
    sizeInBits _ = 32
    {-# INLINE sizeInBits #-}
    indexElem (A _ ba) (I# n) = W32# (indexWord32Array# ba n)
    {-# INLINE indexElem #-}
    readElem = readArrayST readWord32Array# W32#
    {-# INLINE readElem #-}
    writeElem mba n (W32# w) = writeArrayST writeWord32Array# w mba n
    {-# INLINE writeElem #-}
instance ArrayElem Word64 where
    sizeInBits _ = 64
    {-# INLINE sizeInBits #-}
    indexElem (A _ ba) (I# n) = W64# (indexWord64Array# ba n)
    {-# INLINE indexElem #-}
    readElem = readArrayST readWord64Array# W64#
    {-# INLINE readElem #-}
    writeElem mba n (W64# w) = writeArrayST writeWord64Array# w mba n
    {-# INLINE writeElem #-}

-- | return the number of elements of the array.
length :: Array ty -> Int
length (A l _) = I# l
{-# INLINE length #-}

-- | return the numbers of elements in a mutable array
mutableLength :: MutableArray ty st -> Int
mutableLength (MA l _) = I# l
{-# INLINE mutableLength #-}

readArrayST :: (MutableByteArray# st -> Int# -> State# st -> (# State# st, Word# #))
            -> (Word# -> ty)
            -> MutableArray ty st
            -> Int
            -> ST st ty
readArrayST reader constr (MA _ mba) (I# n) =
    ST $ \s1 -> case reader mba n s1 of
                    (# s2, e #) -> (# s2, (constr e) #)
{-# INLINE readArrayST #-}

writeArrayST :: (MutableByteArray# st -> Int# -> Word# -> State# st -> State# st)
             -> Word#
             -> MutableArray ty st
             -> Int
             -> ST st ()
writeArrayST writer val (MA _ mba) (I# n) =
    ST $ \s1 -> (# writer mba n val s1, () #)
{-# INLINE writeArrayST #-}

-- | Copy every cells of an existing array to a new array
copy :: ArrayElem ty => Array ty -> Array ty
copy array = runST (thaw array >>= unsafeFreeze)

-- | Thaw an array to a mutable array.
--
-- the array is not modified, instead a new mutable array is created
-- and every values is copied, before returning the mutable array.
thaw :: ArrayElem ty => Array ty -> ST st (MutableArray ty st)
thaw array = do
    ma <- new (length array)
    copyPrim ma array
    return ma
  where copyPrim (MA _ mba) (A _ ba) = ST $ \st ->
            (# copyByteArray# ba 0# mba 0# (sizeofByteArray# ba) st , () #)
        {-# INLINE copyPrim #-}

    {-
        copyW32 (MA _ mba) (A _ ba) = ST $ \st -> (# loop st 0#, () #)
          where !len = quotInt# (sizeofByteArray# ba) 8#
                loop st n
                    | isTrue# (n ==# len) = st
                    | otherwise           = loop (writeWord32Array# mba n (indexWord32Array# ba n) st) (n +# 1#)
                {-# INLINE loop #-}
        {-# INLINE copyW32 #-}

        copyW64 (MA _ mba) (A _ ba) = ST $ \st -> (# loop st 0#, () #)
          where !len = quotInt# (sizeofByteArray# ba) 8#
                loop st n
                    | isTrue# (n ==# len) = st
                    | otherwise           = loop (writeWord64Array# mba n (indexWord64Array# ba n) st) (n +# 1#)
                {-# INLINE loop #-}
        {-# INLINE copyW64 #-}
    -}
{-# INLINE thaw #-}

-- | Return the element at a specific index from an array.
--
-- If the index @n is out of bounds, an error is raised.
index :: ArrayElem ty => Array ty -> Int -> ty
index array n
    | n < 0 || n >= len = error "Array: index: out of bounds"
    | otherwise         = unsafeIndex array n
  where len = length array
{-# INLINE index #-}

-- | Return the element at a specific index from an array without bounds checking.
--
-- Reading from invalid memory can return unpredictable and invalid values.
-- use 'index' if unsure.
unsafeIndex :: ArrayElem ty => Array ty -> Int -> ty
unsafeIndex = indexElem 
{-# INLINE unsafeIndex #-}

-- | Append 2 arrays together by creating a new bigger array 
append :: ArrayElem ty => Array ty -> Array ty -> Array ty
append a b = runST $ do
    r  <- new (la+lb)
    ma <- unsafeThaw a
    mb <- unsafeThaw b
    copyAt r 0 ma 0 la
    copyAt r la mb 0 lb
    unsafeFreeze r
  where la = length a
        lb = length b

concat :: ArrayElem ty => [Array ty] -> Array ty
concat l = runST $ do
    r <- new (sum $ map length l)
    loop r 0 l
    unsafeFreeze r
  where loop r _ []     = return ()
        loop r i (x:xs) = do
            mx <- unsafeThaw x
            copyAt r i mx 0 lx
            loop r (i+lx) xs
          where lx = length x

-- | read a cell in a mutable array.
--
-- If the index is out of bounds, an error is raised.
read :: ArrayElem ty => MutableArray ty st -> Int -> ST st ty
read array n
    | n < 0 || n >= len = error "Array: read: out of bounds"
    | otherwise         = unsafeRead array n
  where len = mutableLength array
{-# INLINE read #-}

-- | read from a cell in a mutable array without bounds checking.
-- 
-- Reading from invalid memory can return unpredictable and invalid values.
-- use 'read' if unsure.
unsafeRead :: ArrayElem ty => MutableArray ty st -> Int -> ST st ty
unsafeRead = readElem
{-# INLINE unsafeRead #-}

-- | Write to a cell in a mutable array.
--
-- If the index is out of bounds, an error is raised.
write :: ArrayElem ty => MutableArray ty st -> Int -> ty -> ST st ()
write array n
    | n < 0 || n >= len = error "Array: write: out of bounds"
    | otherwise         = unsafeWrite array n
  where len = mutableLength array
{-# INLINE write #-}

-- | write to a cell in a mutable array without bounds checking.
--
-- Writing with invalid bounds will corrupt memory and your program will
-- become unreliable. use 'write' if unsure.
unsafeWrite :: ArrayElem ty => MutableArray ty st -> Int -> ty -> ST st ()
unsafeWrite = writeElem
{-# INLINE unsafeWrite #-}

-- | Create a new mutable array of size @n.
--
-- all the cells are uninitialized and could contains invalid values.
--
-- All mutable arrays are allocated on a 64 bits aligned addresses
-- and always contains a number of bytes multiples of 64 bits.
new :: ArrayElem ty => Int -> ST st (MutableArray ty st)
new (I# n) = newFake undefined
  where newFake :: ArrayElem ty => ty -> ST st (MutableArray ty st)
        newFake ty = ST $ \s1 ->
            case newAlignedPinnedByteArray# bytes 8# s1 of
                (# s2, mba #) -> (# s2, MA n mba #)
          where !(I# szBits) = sizeInBits ty
                !bytes = quotInt# (roundUp (n *# szBits) 64#) 8#
                -- !(I# bytes)   = trace ("new: " ++ show (I# szBits) ++ "bits/elem " ++ show (I# n) ++ " elems " ++ show (I# bytes_) ++ " bytes") (I# bytes_)

                roundUp :: Int# -> Int# -> Int#
                roundUp i d = negateInt# (andI# (negateInt# i)  (negateInt# d))
                {-# INLINE roundUp #-}
        {-# INLINE newFake #-}
{-# INLINE new #-}

-- | Copy a number of elements from an array to another array with offsets
copyAt :: ArrayElem ty
       => MutableArray ty st -- ^ destination array
       -> Int                -- ^ offset at destination
       -> MutableArray ty st -- ^ source array
       -> Int                -- ^ offset at source
       -> Int                -- ^ number of elements to copy
       -> ST st ()
copyAt dst od src os n = loop od 0
  where endIndex = os + n
        loop d i
            | i == endIndex = return ()
            | otherwise     = readElem src i >>= writeElem dst d >> loop (d+1) (i+1)

-- | Create a new array of size @n by settings each cells through the
-- function @f.
create :: ArrayElem ty
       => Int         -- ^ the size of the array
       -> (Int -> ty) -- ^ the function that set the value at the index
       -> Array ty    -- ^ the array created
create n f = runST $ do
    ma <- new n
    iter $ \i -> writeElem ma i (f i)
    unsafeFreeze ma
  where iter z = loop 0
          where loop i | i == n    = return ()
                       | otherwise = z i >> loop (i+1)
                {-# INLINE loop #-}
        {-# INLINE iter #-}

-- | make an array from a list of elements.
fromList :: ArrayElem ty => [ty] -> Array ty
fromList l = runST $ do
    ma <- new len
    iter 0 l $ \i x -> writeElem ma i x
    unsafeFreeze ma
  where len = Prelude.length l
        iter _ [] _ = return ()
        iter i (x:xs) z = z i x >> iter (i+1) xs z

-- | transform an array to a list.
toList :: ArrayElem ty => Array ty -> [ty]
toList a = loop 0
  where len = length a
        loop i | i == len  = []
               | otherwise = indexElem a i : loop (i+1)

-- | update an array by creating a new array with the updates.
--
-- the operation copy the previous array, modify it in place, then freeze it.
update :: ArrayElem ty
       => Array ty
       -> [ (Int, ty) ]
       -> Array ty
update array l = runST $ do
    ma <- thaw array
    doUpdate ma l
    unsafeFreeze ma
  where doUpdate ma l = loop l
          where loop [] = return ()
                loop ((i,v):xs) = write ma i v >> loop xs
                {-# INLINE loop #-}
        {-# INLINE doUpdate #-}

unsafeUpdate :: ArrayElem ty
             => Array ty
             -> [ (Int, ty) ]
             -> Array ty
unsafeUpdate array l = runST $ do
    ma <- thaw array
    doUpdate ma l
    unsafeFreeze ma
  where doUpdate ma l = loop l
          where loop [] = return ()
                loop ((i,v):xs) = unsafeWrite ma i v >> loop xs
                {-# INLINE loop #-}
        {-# INLINE doUpdate #-}

-- | Freeze a mutable array into an array.
--
-- the MutableArray must not be changed after freezing.
unsafeFreeze :: MutableArray ty st -> ST st (Array ty)
unsafeFreeze (MA n mba) = ST $ \s1 ->
    case unsafeFreezeByteArray# mba s1 of
        (# s2, ba #) -> (# s2, A n ba #)
{-# INLINE unsafeFreeze #-}

-- | Thaw an immutable array.
--
-- The Array must not be used after thawing.
unsafeThaw :: ArrayElem ty => Array ty -> ST st (MutableArray ty st)
unsafeThaw (A n ba) = ST $ \st -> (# st, MA n (unsafeCoerce# ba) #)
{-# INLINE unsafeThaw #-}

withConstPtr :: Array ty
             -> (Ptr Word8 -> IO a)
             -> IO a
withConstPtr (A _ a) f =
    f $ Ptr (byteArrayContents# a)

withMutablePtr :: MutableArray ty RealWorld
               -> (Ptr Word8 -> IO a)
               -> IO a
withMutablePtr ma f =
    stToIO (unsafeFreeze ma) >>= flip withConstPtr f
