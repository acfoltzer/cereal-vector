{-# LANGUAGE CPP #-}
-- #define TEST
#ifdef TEST
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
#endif
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

-- | 'Data.Serialize' functions for 'Data.Vector.Generic.Vector'
-- vectors. Orphan instances are provided for 'Data.Vector' and
-- 'Data.Vector.Primitive' vectors.
--
-- Instances are /not/ provided for 'Data.Vector.Unbox' vectors, as
-- they must be declared on an individual basis for each type the
-- vectors may contain. The 'genericGet' and 'genericPut' functions
-- should still work for these vectors without declaring instances.
-- 
-- The serialized format is an 'Int64' representing the
-- length of the 'Vector', followed by the serialized contents of each
-- element.
-- 
-- Note that the instances specialized for 'Data.Vector.Storable' are
-- much more performant for storable vectors.
#ifndef TEST
module Data.Vector.Serialize (genericGetVector, genericPutVector) where
#endif

import Control.Monad

import Data.Int (Int64)
import Data.Serialize (Get, Putter, Serialize(..))
import qualified Data.Vector as V
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Generic as VG

#ifdef TEST
import Data.Serialize (decode, encode)
import Data.Vector.Storable.Serialize ()
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import Test.QuickCheck.All
#endif

-- | Read a 'Data.Vector.Generic.Vector'.
genericGetVector :: (Serialize a, VG.Vector v a) => Get (v a)
genericGetVector = do 
  len64 <- (get :: Get Int64)
  when (len64 > fromIntegral (maxBound :: Int)) $
    fail "Host can't deserialize a Vector longer than (maxBound :: Int)"
  VG.replicateM (fromIntegral len64) get

-- | Write a 'Data.Vector.Generic.Vector'.
genericPutVector :: (Serialize a, VG.Vector v a) => Putter (v a)
genericPutVector v = do
  put $ ((fromIntegral $ VG.length v) :: Int64)
  VG.mapM_ put v

instance (Serialize a) => Serialize (V.Vector a) where
  get = genericGetVector ; put = genericPutVector

instance (Serialize a, VP.Prim a) => Serialize (VP.Vector a) where
  get = genericGetVector ; put = genericPutVector

#ifdef TEST
prop_vec :: [Int] -> Bool
prop_vec xs = (Right xsv) == decode (encode xsv)
  where xsv = V.fromList xs

prop_vec_tuple :: [[Int]] -> [Either [Bool] Double] -> Bool
prop_vec_tuple xs ys = (Right (xsv, ysv)) == decode (encode (xsv, ysv))
  where (xsv, ysv) = (V.fromList xs, V.fromList ys)

prop_prim :: [Int] -> Bool
prop_prim xs = (Right xsv) == decode (encode xsv)
  where xsv = VP.fromList xs

instance Serialize (VU.Vector Int) where
  get = genericGetVector ; put = genericPutVector

prop_unbox :: [Int] -> Bool
prop_unbox xs = (Right xsv) == decode (encode xsv)
  where xsv = VU.fromList xs

prop_storable :: [Int] -> Bool
prop_storable xs = (Right xsv) == decode (encode xsv)
  where xsv = VS.fromList xs

prop_storable_tuple :: [Int64] -> [Double] -> Bool
prop_storable_tuple xs ys = (Right (xsv, ysv)) == decode (encode (xsv, ysv))
  where (xsv, ysv) = (VS.fromList xs, VS.fromList ys)

main :: IO ()
main = void $ $quickCheckAll
#endif