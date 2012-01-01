module Test.QuickCheck.Tsuraan
( randBS
, randBS'
, randLBS
, randLBS'
, randLBS''
, gen
) where

import qualified Crypto.Hash.Tiger as Tiger
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.ByteString as ByteString
import           Data.ByteString ( ByteString )
import           Test.QuickCheck

type LazyByteString = LazyByteString.ByteString

randBS :: Int -> Int -> Gen ByteString
randBS min max = do
  len <- choose (min, max)
  randBS' len

randBS' :: Int -> Gen ByteString
randBS' len = do
  substring <- arbitrary
  if substring
    then do
      offset <- choose (1, 1000)
      bytes <- gen (len+offset)
      return $ ByteString.drop offset bytes
    else
      gen len

randLBS :: Int -> Int -> Gen LazyByteString
randLBS min max = do
  len <- choose (min, max)
  randLBS' len

randLBS' :: Int -> Gen LazyByteString
randLBS' = randLBS'' 1

randLBS'' :: Int -> Int -> Gen LazyByteString
randLBS'' min len = do
  chunks <- mkChunks len []
  return $ LazyByteString.fromChunks chunks
  where
  mkChunks :: Int -> [ByteString] -> Gen [ByteString]
  mkChunks 0 acc = return acc
  mkChunks n acc = do
    l <- choose (1, n)
    bs <- randBS' l
    mkChunks (n-l) (bs:acc)

instance Arbitrary ByteString where
  arbitrary = randBS 1 1000

instance Arbitrary LazyByteString.ByteString where
  arbitrary = randLBS 1 1000

gen :: Int -> Gen ByteString
gen 0 = return ByteString.empty
gen num = do
  words <- sequence $ replicate 24 arbitrary
  let init = ByteString.pack words
  let chunks = iterate Tiger.hash init
  let needed = 1 + div (num-1) 24
  return $ ByteString.take num $ ByteString.concat $ take needed chunks

