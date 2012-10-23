{-# LANGUAGE CPP #-}
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
module Data.Vector.Serialize (
    genericGetVector
  , genericPutVector
  ) where

import Control.Monad

import Data.Int (Int64)
import Data.Serialize (Get, Putter, Serialize(..))
import qualified Data.Vector as V
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Generic as VG

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
