{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
-- | This module provides BLAS library functions for vectors of
-- single precision floating point numbers.
module Numerical.BLAS.Single(
   sdot_zip,
   sdot,
   sasum,
   snrm2,
    ) where

import Data.Vector.Storable(Vector)
import qualified Data.Vector.Storable as V

{- | O(n) compute the dot product of two vectors using zip and fold
-}
sdot_zip :: Vector Float -- ^ The vector u
    -> Vector Float      -- ^ The vector v
    -> Float             -- ^ The dot product u . v
sdot_zip u = V.foldl (+) 0 . V.zipWith (*) u


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
   | incx /=1 || incy /= 1  = sumProdsInc
   | n < 5     = sumprods 0 0
   | m == 0    = unrolled 0 0
   | otherwise =
        let subtotal = sumprods 0 0
        in  unrolled subtotal m
    where
        m :: Int
        m = n `mod` 5
        {-# INLINE sumprods #-}
        sumprods !c !i
            | i < m  = sumprods (c + sx `V.unsafeIndex` i * sy `V.unsafeIndex` i) (i+1)
            | otherwise = c
        {-# INLINE sumProdsInc #-}
        sumProdsInc = sumprodloop 0 0 (firstIndex incx) (firstIndex incy)
           where
               {-# INLINE sumprodloop #-}
               sumprodloop !c !k !i !j
                  | k < n = sumprodloop (c + sx `V.unsafeIndex` i * sy`V.unsafeIndex`j) (k+1) (i+incx) (j+incy)
                  | otherwise = c
        -- hyloL :: ( a -> Maybe (b,a)) -> (c -> b -> c) -> c -> a -> c
        {-# INLINE [1] unrolled #-}
        unrolled !c !i
           | i>=n = c
           | otherwise =
               let i'  = i+5
                   i1  = i+1
                   i2  = i+2
                   i3  = i+3
                   i4  = i+4
                   c'  = c + (sx `V.unsafeIndex` i)*(sy `V.unsafeIndex` i)
                       + (sx `V.unsafeIndex` i1)*(sy `V.unsafeIndex` i1)
                       + (sx `V.unsafeIndex` i2)*(sy `V.unsafeIndex` i2)
                       + (sx `V.unsafeIndex` i3)*(sy `V.unsafeIndex` i3)
                       + (sx `V.unsafeIndex` i4)*(sy `V.unsafeIndex` i4)
               in  unrolled c' i'
   -- O(1) - Compute the starting index of an iterative vector traversal
   -- given the length of the vector and the iterative step size.
        firstIndex :: Int -- ^ the iterative step
                  -> Int -- ^ The index of the first iterative step
        {-# INLINE firstIndex #-}
        firstIndex !inc
            | inc>0     = 0
            | otherwise = (1-n)*inc


{- | O(n) sasum computes the sum of the absolute value of elements drawn a
  vector according to the following specification

@
   sdot n u incx = sum { abs u[i*incx]   | i<=[0..n-1] }
@

  The elements selected from the vector are controlled by the parameters
  n and incx.   The parameter n determines the number of summands, while
  the parameter incx determines the spacing between selected elements.

  No bound checks are performed.   The calling program should ensure that:

@
    length u >= (1 + (n-1)*abs(incx))
@
-}
sasum :: Int -- ^ The number of summands
   -> Vector Float -- ^ the vector u
   -> Int          -- ^ the space between elements drawn from u
   -> Float        -- ^ The sum of the product of the elements.
sasum n sx !incx
  | n < 1 || incx <1 = 0
  | incx /=1         = sumAbsInc (n*incx)
  | n < 5            = sumAbs 0 0
  | m == 0           = unrolled 0 0
  | otherwise        = unrolled (sumAbs 0 0) m
   where
       m :: Int
       m = n `mod` 6
       {-# INLINE sumAbs #-}
       sumAbs !c !i
           | i < m  = sumAbs (c + abs (sx `V.unsafeIndex` i)) (i+1)
           | otherwise = c
       {-# INLINE sumAbsInc #-}
       sumAbsInc !imax = sumabsloop 0 0
          where
              {-# INLINE sumabsloop #-}
              sumabsloop !c !i
                 | i < imax = sumabsloop (c + abs (sx `V.unsafeIndex` i)) (i+incx)
                 | otherwise = c
       {-# INLINE [1] unrolled #-}
       unrolled !c !i
          | i>=n = c
          | otherwise =
              let i'  = i+6
                  i1  = i+1
                  i2  = i+2
                  i3  = i+3
                  i4  = i+4
                  i5  = i+5
                  c'  = c + (abs $ sx `V.unsafeIndex` i)
                      + (abs $ sx `V.unsafeIndex` i1)
                      + (abs $ sx `V.unsafeIndex` i2)
                      + (abs $ sx `V.unsafeIndex` i3)
                      + (abs $ sx `V.unsafeIndex` i4)
                      + (abs $ sx `V.unsafeIndex` i5)
              in  unrolled c' i'

{- | O(n) sasum computes the sum of the squares of elements drawn a
vector according to the following specification

@
 sdot n u incx = sum { u[i*incx] ^2   | i<=[0..n-1] }
@

The elements selected from the vector are controlled by the parameters
n and incx.   The parameter n determines the number of summands, while
the parameter incx determines the spacing between selected elements.

Note: The BLAS implementation computes the scaled sum of squares such that
1.0 < ssq < 2*n and scale is the maximum of either 1.0 or the largest absolute
value of the elments of u.

No bound checks are performed.   The calling program should ensure that:

@
  length u >= (1 + (n-1)*abs(incx))
@
-}
snrm2 :: Int -- ^ The number of summands
 -> Vector Float -- ^ the vector u
 -> Int          -- ^ the space between elements drawn from u
 -> Float        -- ^ The l2 norm of the vector u
snrm2 !n sx !incx
    | n<1 || incx<1 = 0.0
    | n==1          = abs $ sx `V.unsafeIndex` 0
    | otherwise     = slassq (incx*(n-1))
    where
    slassq !imax = slassq_loop 0 0.0 1.0
        where
        slassq_loop :: Int -> Float -> Float -> Float
        {-# INLINE slassq_loop #-}
        slassq_loop !i !scale !ssq
            | i > imax = scale*sqrt ssq
            | otherwise =
                let xi = sx `V.unsafeIndex` i
                    f:: Float -> Float
                    {-# INLINE f #-}
                    f x = case scale < x of
                        True  -> slassq_loop (i+incx) x (1.0 + ssq * (scale/x)**2.0)
                        False -> slassq_loop (i+incx) scale (ssq + (x/scale)**2.0)
                in  case compare xi 0.0 of
                       EQ -> slassq_loop (i+incx) scale ssq
                       LT -> f (negate xi)
                       GT -> f xi
