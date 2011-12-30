module Test.QuickCheck.Tsuraan
( randBS
, randBS'
) where

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
      bytes <- sequence $ replicate (len+offset) arbitrary
      return $ ByteString.drop offset $ ByteString.pack bytes
    else do
      bytes <- sequence $ replicate len arbitrary
      return $ ByteString.pack bytes

randLBS :: Int -> Int -> Gen LazyByteString
randLBS min max = do
  len <- choose (min, max)
  randLBS' len

randLBS' :: Int -> Gen LazyByteString
randLBS' len = do
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

