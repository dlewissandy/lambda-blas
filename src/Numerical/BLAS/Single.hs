-- | This module provides BLAS library functions for vectors of
-- single precision floating point numbers.
module Numerical.BLAS.Single(
   sdot_zip,
   sdot,
    ) where

import Data.Vector.Unboxed(Vector)
import qualified Data.Vector.Unboxed as V

{- | O(n) compute the dot product of two vectors using zip and fold
-}
sdot_zip :: Vector Float -- ^ The vector u
    -> Vector Float      -- ^ The vector v
    -> Float             -- ^ The dot product u . v
sdot_zip u v = V.foldl (+) 0 $ V.zipWith (*) u v


{- | O(n) sdot computes the sum of the products of elements drawn from two
   vectors according to the following specification:

@
   sdot n u incx v incy = sum { u[f incx k] * v[f incy k] | k<=[0..n-1] }
   where
   f inc k | inc > 0  = inc * k
           | inc < 0  = (1-n+k)*inc
@

  The elements selected from the two vectors are controlled by the parameters
  n, incx and incy.   The parameter n determines the number of summands, while
  the parameters incx and incy determine the spacing between selected elements
  and the direction which the vectors are traversed.  When both incx and incy
  are unity and n is the length of both vectors then
  sdot corresponds to the dot product of two vectors.
-}
sdot :: Int -- ^ The number of summands
    -> Vector Float -- ^ the vector u
    -> Int          -- ^ the space between elements drawn from u
    -> Vector Float -- ^ the vector v
    -> Int          -- ^ the space between elements drawn from v
    -> Float        -- ^ The sum of the product of the elements.
sdot n sx incx sy incy
   | n < 1                  = error "Encountered zero or negative length vector"
   | incx == 1 && incy == 1 = sdot_unrolled
   | otherwise              = sdot_loop
   where
   sdot_unrolled :: Float
   {-# INLINE sdot_unrolled #-}
   sdot_unrolled = case ( n `mod` 5) of
       0 -> sdot_unrolled_mod5 0 0
       m -> sdot_unrolled_not_mod5 m
   sdot_unrolled_not_mod5 :: Int -> Float
   {-# INLINE sdot_unrolled_not_mod5 #-}
   sdot_unrolled_not_mod5 m
      | n < 5     = go 0 0
      | otherwise = sdot_unrolled_mod5 imax $! go 0 0
      where
      imax = min n m
      go :: Int -> Float -> Float
      {-# INLINE go #-}
      go i accum
         | i>=imax   = accum
         | otherwise = go (succ i) $ accum + (sx V.! i)*(sy V.! i)
   sdot_unrolled_mod5 :: Int -> Float -> Float
   {-# INLINE sdot_unrolled_mod5 #-}
   sdot_unrolled_mod5 i accum
       | i >= n    = accum
       | otherwise = sdot_unrolled_mod5 (i+5) $ accum + (sx V.! i)*(sy V.! i)
           + (sx V.! (i+1))*(sy V.! (i+1)) + (sx V.! (i+2))*(sy V.! (i+2))
           + (sx V.! (i+3))*(sy V.! (i+3))+ (sx V.! (i+4))*(sy V.! (i+4))
   sdot_loop :: Float
   {-# INLINE sdot_loop #-}
   sdot_loop = go ix iy 0 0
      where
      ix = if incx<0 then (-n+1)*incx else 0
      iy = if incy<0 then (-n+1)*incy else 0
      go j k i accum
         | i >= n    = accum
         | otherwise = go (j+incx) (k+incy) (succ i) $ accum+(sx V.! j)*(sy V.! k)
