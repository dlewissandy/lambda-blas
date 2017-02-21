{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ViewPatterns #-}
-- | This module provides BLAS library functions for vectors of
-- single precision floating point numbers.
module Numerical.BLAS.Single(
   isamax,
   sdot_zip,
   sdot_list,
   sdot_stream,
   sdot,
   sasum,
   snrm2,
   sdsdot,
    ) where

import Data.Vector.Storable(Vector)
import qualified Data.Vector.Storable as V
import Data.Vector.Fusion.Bundle.Size(Size(..))
import Data.Vector.Fusion.Bundle.Monadic(fromStream)
import Data.Vector.Fusion.Stream.Monadic(Stream(..),Step(..))
import Data.Vector.Generic(unstream)
import GHC.Exts
import Data.List

{- | O(n) compute the dot product of two vectors using zip and fold
-}
sdot_zip :: Int
    -> Vector Float -- ^ The vector u
    -> Vector Float      -- ^ The vector v
    -> Float             -- ^ The dot product u . v
sdot_zip !n u = V.foldl' (+) 0 . V.unsafeTake n . V.zipWith (*) u

{- | O(n) sdot_list function computes the sum of the products of elements
   drawn from two vectors according to the following specification:

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
  are unity and n is the length of both vectors then sdot corresponds to the dot
  product of two vectors.

  This function is semantically equivalent to the "sdot" and "sdot_stream"
  functions, but has been written using build/fold fusion.
-}

sdot_list :: Int
    -> [Float] -- ^ The vector u
    -> Int     -- ^ The increment for u
    -> [Float] -- ^ The vector v
    -> Int     -- ^ The increment for v
    -> Float   -- ^ The sum of products from u and v
sdot_list !n u !incx v !incy =
    case compare incx 0 of
        GT -> case compare incy 0 of
            GT -> if (incx*incy==1)
                    then foldl' (+) 0 $ take n $ zipWith (*) u v
                    else foldl' (+) 0 $ getProds u v
            LT -> foldl' (+) 0 $ getProds u vs'
            _  -> foldl' (+) 0 $ getProds u (replicate n v0)
        LT -> case compare incy 0 of
            LT -> if (incx*incy==1)
                    then foldr (+) 0 $ take n $ zipWith (*) u v
                    else foldr (+) 0 $ getProds u v
            GT -> foldl' (+) 0 $ getProds us' v
            _  -> foldl' (+) 0 $ getProds us' (replicate n v0)
        _ -> case compare incy 0 of
            LT -> foldr  (+) 0 $ getProds (replicate n u0) v
            GT -> foldl' (+) 0 $ getProds (replicate n u0) v
            EQ -> foldl' (+) 0 $ replicate n (u0*v0)
    where
        aincx = abs incx
        aincy = abs incy
        u0 = head u
        v0 = head v
        us'= reverse $ take (1+(n-1)*aincx) u
        vs'= reverse $ take (1+(n-1)*aincy) v
        {-# INLINE getProds #-}
        getProds xx yy = build ( \ c b ->
           let go xs ys !i = case xs of
                [] -> b
                (x:_) -> case ys of
                    [] -> b
                    (y:_) -> case i of
                        0 -> b
                        _ -> c (x*y) $! go (drop aincx xs) (drop aincy ys) (i-1)
           in go xx yy n)

{- | O(n) sdot_stream function computes the sum of the products of elements
   drawn from two vectors according to the following specification:

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
  are unity and n is the length of both vectors then sdot corresponds to the dot
  product of two vectors.

  This function is semantically equivalent to the "sdot" and "sdot_list"
  functions, but has been written using stream fusion to produce efficient,
  beutiful code.
-}
sdot_stream :: Int -> V.Vector Float -> Int -> V.Vector Float -> Int -> Float
sdot_stream !n u !incx v !incy =
    case compare incx 0 of
        GT -> case compare incy 0 of
            GT -> sumprods xs ys
            LT -> sumprods xs ys'
            EQ -> V.foldl' (+) 0 $ V.map (* v0) xs
        LT -> case compare incy 0 of
            LT -> sumprods xs' ys'
            GT -> sumprods xs' ys
            EQ -> V.foldl' (+) 0 $ V.map (* v0) xs'
        EQ -> case compare incy 0 of
            LT -> V.foldl' (+) 0 $ V.map (* u0) ys'
            GT -> V.foldl' (+) 0 $ V.map (* u0) ys
            EQ -> V.foldl' (+) 0 $ V.replicate n (v0*u0)
    where
        {-# INLINE sumprods #-}
        sumprods x = V.foldl' (+) 0 . V.zipWith (*) x
        xs  = sample n u incx
        ys  = sample n v incy
        xs' = samplerev n u incx
        ys' = samplerev n v incy
        v0  = v `V.unsafeIndex` 0
        u0  = u `V.unsafeIndex` 0


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

  This function is semantically equivalent to the "sdot_stream" and "sdot_list"
  functions, but faithfully implements the loop unrolling strategy used by
  the FORTRAN implementation.
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
        sumProdsInc = sumprodloop 0 0 (firstIndex n incx) (firstIndex n incy)
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
firstIndex :: Int -> Int -- ^ the iterative step
          -> Int -- ^ The index of the first iterative step
{-# INLINE firstIndex #-}
firstIndex !n !inc
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
 snrm2 n u incx = sum { u[i*incx] ^2   | i<=[0..n-1] }
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


{- | O(n) isamax computes the index of the element of a vector having the
largest absolute value according to the following specification.

@
    isamax n u incx = i
    where
        zs = [ abs u[i*incx]   | i<=[0..n-1] ]
        a  = max zs
        Just i = findIndex a zs
@

 The elements selected from the vector are controlled by the parameters
 n and incx.   The parameter n determines the number of summands, while
 the parameter incx determines the spacing between selected elements.

 No bound checks are performed.   The calling program should ensure that:

@
   length u >= (1 + (n-1)*abs(incx))
@
-}
isamax :: Int -> V.Vector Float -> Int -> Int
isamax !n sx !incx
   | n < 1 || incx < 1 = -1
   | n == 1            = 0
   | incx == 1         = findmax1 0 (abs $ sx `V.unsafeIndex` 0) 1
   | otherwise         = findmax 0 (abs $ sx `V.unsafeIndex` 0) 1 incx
   where
       findmax1 !k !c !i
          | i >= n  = k
          | otherwise =
              let xi = sx `V.unsafeIndex` i
                  f x = case x > c of
                       True -> findmax1 i x (i+1)
                       False -> findmax1 k c (i+1)
              in  case compare xi 0.0 of
                      EQ -> findmax1 k c (i+1)
                      LT -> f $ negate xi
                      GT -> f xi
       findmax !k !c !i !ix
          | i >= n  = k
          | otherwise =
              let xi = sx `V.unsafeIndex` ix
                  f x = case x > c of
                       True -> findmax i x (i+1) (ix+incx)
                       False -> findmax k c (i+1) (ix+incx)
              in  case compare xi 0.0 of
                      EQ -> findmax k c (i+1) (ix+incx)
                      LT -> f $ negate xi
                      GT -> f xi


{- | O(n) sdot computes the sum of the products of elements drawn from two
 vectors according to the following specification:

@
 sdsdot n u incx v incy = sum { u[f incx k] * v[f incy k] | k<=[0..n-1] }
 where
 f inc k | inc > 0  = inc * k
         | inc < 0  = (1-n+k)*inc
@
The elements selected from the two vectors are controlled by the parameters
n, incx and incy.   The parameter n determines the number of summands, while
the parameters incx and incy determine the spacing between selected elements
and the direction which the vectors are traversed.  When both incx and incy
are unity and n is the length of both vectors then
sdsdot corresponds to the dot product of two vectors.

NOTE: The summation is performed using double precision floating point and the
result is coerced to a single immediatly prior to returing the value.
-}
sdsdot :: Int -> Float -> Vector Float -> Int -> Vector Float -> Int -> Float
sdsdot n (floatToDouble -> sb) sx !incx sy !incy
   | n< 1                   = doubleToFloat sb
   | incx==incy && incx > 0 = whenPositiveEqualInc (n*incx)
   | otherwise              = whenUnequalOrNegIncs 0 sb (firstIndex n incx) (firstIndex n incy)
   where
   whenPositiveEqualInc :: Int -> Float
   {-# INLINE whenPositiveEqualInc #-}
   whenPositiveEqualInc !imax = whenPositiveEqualInc_loop 0 sb
       where
       whenPositiveEqualInc_loop :: Int -> Double -> Float
       {-# INLINE whenPositiveEqualInc_loop #-}
       whenPositiveEqualInc_loop !i !c
            | i>=imax   = doubleToFloat c
            | otherwise =
                let c' = c + (floatToDouble ( sx `V.unsafeIndex` i))
                           * (floatToDouble ( sy `V.unsafeIndex` i))
                in whenPositiveEqualInc_loop (i+incx) c'
   whenUnequalOrNegIncs :: Int -> Double -> Int -> Int -> Float
   {-# INLINE whenUnequalOrNegIncs #-}
   whenUnequalOrNegIncs !i !c !kx !ky
       | i >= n    = doubleToFloat c
       | otherwise =
           let c' = c + (floatToDouble $ sx `V.unsafeIndex` kx)
                      * (floatToDouble $ sy `V.unsafeIndex` ky)
           in  whenUnequalOrNegIncs (i+1) c' (kx+incx) (ky+incy)


--- UTILITIES --
-- Move these to an appropriate module

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
sample :: Int -> V.Vector Float -> Int -> V.Vector Float
{-# INLINE sample #-}
sample !n u !inc = unstream $ fromStream (Stream go 0) (Exact n)
    where
    go !ix
       | ix > imax = return Done
       | otherwise = return $ Yield (u `V.unsafeIndex` ix) (ix+inc)
    imax = (n-1)*inc

-- | O(n), downstream fusable.   Sample a vector in even intevals, collecting the
-- first n elements into a vector according to the following specification:
--
-- @
-- sample n u inc = fromList $ reverse [ u!(i*incx) | i<-[0..n-1]]
-- @
-- The vector must have at least (n-1)*inc elements in it.  This condition is
-- not checked, and must be verified by the calling program
samplerev :: Int -> V.Vector Float -> Int -> V.Vector Float
{-# INLINE samplerev #-}
samplerev !n u !inc = unstream $ fromStream (Stream go ((1-n)*inc)) (Exact n)
    where
    go !ix
       | ix < 0    = return Done
       | otherwise = return $ Yield (u `V.unsafeIndex` ix) (ix+inc)
