{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Crypto.Signature.Ed25519
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : portable
--
-- A port of ED25519 in pure Haskell for fun and profits.
--
module Crypto.Signature.Ed25519
    ( SecretKey(..)
    , PublicKey(..)
    , Signature(..)
    , publicKey
    , signature
    , verify
    , selfTest
    ) where

import qualified "cryptohash" Crypto.Hash.SHA512 as SHA512
import Data.Word
import Data.Bits ((.&.), testBit)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B

newtype SecretKey = SecretKey ByteString
newtype PublicKey = PublicKey { unPublicKey :: ByteString } deriving Eq
newtype Signature = Signature { unSignature :: ByteString } deriving Eq

type Point = (Integer, Integer)

hash = SHA512.hash

b = 256
q = 2 ^ 255 - 19
l = 2 ^ 252 + 27742317777372353535851937790883648493
d = (-121665) * inv 121666
_I = expMod 2 ((q-1) `div` 4) q

expMod :: Integer -> Integer -> Integer -> Integer
expMod b e m
    | e == 0    = 1
    | otherwise =
        let t = (expMod b (e `div` 2) m ^ 2) `mod` m
         in if odd e then (t * b) `mod` m else t

inv x = expMod x (q - 2) q

xRecover :: Integer -> Integer
xRecover y = if odd x' then q - x' else x'
  where
    y2 = y*y
    xx = (y2-1) * inv (d*y2+1)
    x  = expMod xx ((q+3) `div` 8) q
    x' | ((x*x - xx) `mod` q) /= 0 = (x * _I) `mod` q
       | otherwise                 = x

base = (xRecover by `mod` q, by `mod` q)
  where by = 4 * inv(5)

edwards :: Point -> Point -> Point
edwards (x1, y1) (x2, y2) = (x3 `mod` q, y3 `mod` q)
  where x3 = (x1*y2+x2*y1) * inv (1+z)
        y3 = (y1*y2+x1*x2) * inv (1-z)
        z  = d*x1*x2*y1*y2

scalarMult p e
    | e == 0    = (0, 1)
    | otherwise =
        let q  = scalarMult p (e `div` 2)
            q' = edwards q q
         in if odd e then edwards q' p else q'

encodeFe y = B.pack (loop 0 y)
  where loop :: Int -> Integer -> [Word8]
        loop w n
            | w == 248  = [fromIntegral n]
            | otherwise = let (d,r) = n `divMod` 256 in fromIntegral r : loop (w+8) d

encodePoint :: Point -> ByteString
encodePoint (x, y) = B.pack $ loop 0 y
  where loop :: Int -> Integer -> [Word8]
        loop w n
            | w == 248  = [fromIntegral (n .&. 0x7f) + if odd x then 0x80 else 0]
            | otherwise = let (d,r) = n `divMod` 256 in fromIntegral r : loop (w+8) d

bit h i = (B.index h idx) `testBit` r
  where (idx, r) = i `divMod` 8

-- | transform a message into a integer
hint m = let h = hash m in sum [ (if bit h i then 2^i else 0) | i <- [0..(2*b-1)] ]

isOnCurve :: Point -> Bool
isOnCurve (x, y) = ((-x*x + y*y - 1 - d*x*x*y*y) `mod` q) == 0

decodeFe s = sum [ (if bit s i then 2^i else 0) | i <- [0..(b-1)] ]

decodePoint s =
    let y  = sum [ (if bit s i then 2^i else 0) | i <- [0..(b-2)] ] 
        x  = xRecover y
        -- x' = if testBit x 0 == bit s (b-1) then q - x else x
        p  = (x,y)
     in if isOnCurve p then p else error "decoding point not on curve"

-- | Compute the public key associated with a secret key
publicKey (SecretKey sk) =
    let h  = hash sk
        a  = 2 ^ (b-2) + sum [ (if bit h i then 2^i else 0) | i <- [3..b-3] ]
        pA = scalarMult base a
     in PublicKey $ encodePoint pA

-- | Sign a message given a keypair
signature m (SecretKey sk) (PublicKey pk) =
    let h  = hash sk
        a  = 2 ^ (b-2) + sum [ (if bit h i then 2^i else 0) | i <- [3..b-3] ]
        r  = hint ((B.take 32 $ B.drop 32 h) `B.append` m)
        pR = scalarMult base r
        pS = (r + hint (B.concat [encodePoint pR, pk, m]) * a) `mod` l
     in Signature (encodePoint pR `B.append` encodeFe pS)

-- | Verify the signature given a message and a publicKey.
verify :: Signature -> ByteString -> PublicKey -> Bool
verify (Signature sig) m (PublicKey pk) =
    let pR = decodePoint s1
        pA = decodePoint pk
        pS = decodeFe s2
        h  = hint $ B.concat [ encodePoint pR, pk, m ]
     in scalarMult base pS == edwards pR (scalarMult pA h)
  where (s1, s2) = B.splitAt 32 sig

-- | self Test
selfTest = publicKey tSecret == tPublic && signature tMsg tSecret tPublic == tSig && verify tSig tMsg tPublic
  where tSecret = SecretKey "\xb1\x8e\x1d\x00\x45\x99\x5e\xc3\xd0\x10\xc3\x87\xcc\xfe\xb9\x84\xd7\x83\xaf\x8f\xbb\x0f\x40\xfa\x7d\xb1\x26\xd8\x89\xf6\xda\xdd"
        tPublic = PublicKey "\x77\xf4\x8b\x59\xca\xed\xa7\x77\x51\xed\x13\x8b\x0e\xc6\x67\xff\x50\xf8\x76\x8c\x25\xd4\x83\x09\xa8\xf3\x86\xa2\xba\xd1\x87\xfb"
        tMsg    = "\x91\x6c\x7d\x1d\x26\x8f\xc0\xe7\x7c\x1b\xef\x23\x84\x32\x57\x3c\x39\xbe\x57\x7b\xbe\xa0\x99\x89\x36\xad\xd2\xb5\x0a\x65\x31\x71\xce\x18\xa5\x42\xb0\xb7\xf9\x6c\x16\x91\xa3\xbe\x60\x31\x52\x28\x94\xa8\x63\x41\x83\xed\xa3\x87\x98\xa0\xc5\xd5\xd7\x9f\xbd\x01\xdd\x04\xa8\x64\x6d\x71\x87\x3b\x77\xb2\x21\x99\x8a\x81\x92\x2d\x81\x05\xf8\x92\x31\x63\x69\xd5\x22\x4c\x99\x83\x37\x2d\x23\x13\xc6\xb1\xf4\x55\x6e\xa2\x6b\xa4\x9d\x46\xe8\xb5\x61\xe0\xfc\x76\x63\x3a\xc9\x76\x6e\x68\xe2\x1f\xba\x7e\xdc\xa9\x3c\x4c\x74\x60\x37\x6d\x7f\x3a\xc2\x2f\xf3\x72\xc1\x8f\x61\x3f\x2a\xe2\xe8\x56\xaf\x40"
        tSig    = Signature "\x6b\xd7\x10\xa3\x68\xc1\x24\x99\x23\xfc\x7a\x16\x10\x74\x74\x03\x04\x0f\x0c\xc3\x08\x15\xa0\x0f\x9f\xf5\x48\xa8\x96\xbb\xda\x0b\x4e\xb2\xca\x19\xeb\xcf\x91\x7f\x0f\x34\x20\x0a\x9e\xdb\xad\x39\x01\xb6\x4a\xb0\x9c\xc5\xef\x7b\x9b\xcc\x3c\x40\xc0\xff\x75\x09"
{-
main = do
    let s   = SecretKey $ B.replicate 32 0x61
        p   = publicKey s
        sig = signature "ABC" s p
        v   = verify sig "ABC" p
        v2  = verify sig "ABCD" p
    putStrLn $ ("valid message should be True   : " ++ show v)
    putStrLn $ ("invalid message should be False: " ++ show v2)
-}
