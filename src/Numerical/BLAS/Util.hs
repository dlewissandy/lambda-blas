{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
module Numerical.BLAS.Util(
    zipElemsWith,
    sampleIndices, sampleIndicesRev,
    sampleElems,updateElems,
    floatToDouble,doubleToFloat
    ) where

import qualified Data.Vector.Storable as V
import Data.Vector.Fusion.Bundle.Size(Size(..))
import Data.Vector.Fusion.Bundle.Monadic(fromStream)
import Data.Vector.Fusion.Stream.Monadic(Stream(..),Step(..))
import Data.Vector.Generic(unstream)
import GHC.Exts


floatToDouble :: Float -> Double
{-# INLINE floatToDouble #-}
floatToDouble (F# f) = case float2Double# f of
    d -> D# d

doubleToFloat :: Double -> Float
{-# INLINE doubleToFloat #-}
doubleToFloat (D# d) = case double2Float# d of
    f -> F# f


-- | O(n), downstream fusable.   Sample a vector in even intevals, collecting the
-- first n elements into a vector according to the following specification:
--
-- @
-- sample n u inc = fromList [ u!(i*incx) | i<-[0..n-1]]
-- @
-- The vector must have at least (n-1)*inc elements in it.  This condition is
-- not checked, and must be verified by the calling program
sampleIndices :: Int -> Int -> V.Vector Int
{-# INLINE sampleIndices #-}
sampleIndices !n !inc = unstream $ fromStream (Stream go 0) (Exact n)
    where
    go !ix
       | ix > imax = return Done
       | otherwise = return $ Yield ix (ix+inc)
    imax = (n-1)*inc

-- | O(n), downstream fusable.   A list of indices sampled in uniform increments
-- but in reverse order
--
-- @
-- sample n u inc = fromList $ reverse [ (i*incx) | i<-[0..n-1]]
-- @
sampleIndicesRev :: Int -> Int -> V.Vector Int
{-# INLINE sampleIndicesRev #-}
sampleIndicesRev !n !inc = unstream $ fromStream (Stream go ((1-n)*inc)) (Exact n)
    where
    go !ix
       | ix < 0    = return Done
       | otherwise = return $ Yield ix (ix+inc)


-- | O(n), downstream fusable.   Sample a vector in iniform intevals, collecting
-- the first n elements into a vector according to the following specification:
-- @
-- sampleElems n sx 1 = take n sx
-- sampleElems n sx 0 = replicate n (sx ! 0)
-- sampleElems n sx (-1) = reverse $ take n sx
-- sampleElems n sx inc
--    | inc > 0   = map ( xs! ) $ sampleIndices n inc
--    | otherwise = map ( xs! ) $ sampleIndicesRev n inc
-- @
-- The vector must have at least (n-1)*(abs inc) elements in it.  This condition
-- is not checked, and must be verified by the calling program.
sampleElems :: (V.Storable a)
    => Int
    -> V.Vector a
    -> Int
    -> V.Vector a
{-# INLINE sampleElems #-}
sampleElems !n sx !incx = case incx > 0 of
    True -> case incx of
        1    -> V.unsafeTake n sx
        _    -> V.map (sx `V.unsafeIndex`) $ sampleIndices n incx
    _    -> case incx of
        0    -> V.replicate n $ V.unsafeIndex sx 0
        (-1) -> V.reverse $ V.unsafeTake n sx
        _    -> V.map (sx `V.unsafeIndex`) $ sampleIndicesRev n incx

-- | O(n) -- Zip elements drawn from two vectors according to the following
-- specification:
--
-- @
-- zipElemsWith f n sx incx sy incy
--    = zipWith f (sampleElems n sx incx) (sample n sy incy)
-- @
-- The vector sx(sy) must have at least (n-1)*(abs incx(incy)) elements in it.
-- this condition is not checked and must be verified by the calling program
zipElemsWith :: (V.Storable a, V.Storable b, V.Storable c)
    => ( a -> b -> c )
    -> Int
    -> V.Vector a
    -> Int
    -> V.Vector b
    -> Int
    -> V.Vector c
{-# INLINE zipElemsWith #-}
zipElemsWith f !n sx !incx sy !incy
    = V.zipWith f (sampleElems n sx incx) (sampleElems n sy incy)

{-  This utility function combines two vectors according to the following
   specification:
@
(updateElems f n sx incx sy incy)!j = case (i*abs incx) == j of
    False -> sx!j
    True  -> f (sx!j) (sy!k)
    where
        k | signum incx == signum incy = i*abs incy
          | otherwise                  = (n-1-i)*abs incy
@

Both vectors sx and sy must have sufficient elements;  This condition is not
checked and must be verified by the calling program.
-}
updateElems :: (V.Storable a, V.Storable b)
    => ( a -> b -> a )
    -> Int
    -> V.Vector a
    -> Int
    -> V.Vector b
    -> Int
    -> V.Vector a
{-# INLINE updateElems #-}
updateElems f !n sx !incx sy !incy =
    case n of
        0 -> sx
        _ -> case incx > 0 of
            True -> V.unsafeUpdate_ sx ixs $ zipElemsWith f n sx incx sy incy
            False -> V.unsafeUpdate_ sx ixs' $ zipElemsWith f n sx incx sy incy
    where
        ixs = sampleIndices n $ abs incx
        ixs'= sampleIndicesRev n incx
