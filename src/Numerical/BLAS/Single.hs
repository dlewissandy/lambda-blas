{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
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
   srotg,
   srotmg,
   sscal,
   scopy,copyHelper
    ) where

import Numerical.BLAS.Types

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
    -> Int
    -> [Float]      -- ^ The vector v
    -> Int
    -> Float             -- ^ The dot product u . v
sdot_list !n u !incx v !incy =
    case compare incx 0 of
        GT -> case compare incy 0 of
            GT -> if (incx*incy==1)
                    then foldl' (+) 0 $ take n $ zipWith (*) u v
                    else foldl' (+) 0 $ getProds u v
            LT -> foldl' (+) 0 $ getProds u vs'
            _ -> foldl' (+) 0 $ getProds u (replicate n v0)
        LT -> case compare incy 0 of
            LT -> if (incx*incy==1)
                    then foldr (+) 0 $ take n $ zipWith (*) u v
                    else foldr (+) 0 $ getProds u v
            GT -> foldl' (+) 0 $ getProds us' v
            _  -> foldl' (+) 0 $ getProds us' (replicate n v0)
        _ -> case compare incy 0 of
            LT -> foldr (+) 0 $ getProds (replicate n u0) v
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
        getProds xx yy = build (\ c b ->
            let go !i xs ys
                    | i==n = b
                    | otherwise =
                        let x=head xs
                            y=head ys
                            xy = x*y
                        in  c xy $! go (i+1) (drop aincx xs) (drop aincy ys)
            in  go 0 xx yy)

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

{- | O(n) sscal multiply a scalar by n elements drawn from a vector
according to the following specification:

@
   forall incx>0, n>0, 0<=i<=(n-1)*incx
     (sscal n a u inc)!i ==
         if i `mod` inc == 0
            then a*u!i
            else = u!i
@

  The elements selected from the vector are controlled by the parameters
  n and incx.   The parameter n determines the number of elements, while
  the parameter incx determines the spacing between selected elements.

  No bound checks are performed.   The calling program should ensure that:

@
    length u >= (1 + (n-1)incx)
@
-}
sscal :: Int -> Float -> V.Vector Float -> Int -> V.Vector Float
sscal n a u incx =
   case n<1 of
       True -> u
       False -> case compare incx 1 of
           LT -> u
           EQ -> let (l,r) = V.splitAt n u in V.map (*a) l V.++ r
           GT -> V.unsafeUpdate_ u (sampleIndices n incx) (V.map (*a) $ sample n u incx)

{- | O(n) copy n elements from one vector into another according to the
following specification

@
   forall incx>0, n>0, 0<=j<=length v 0<=i<n
     (sscal n sx incx sy incy)!i =
         if j==i*incy
            then if (sign incx) == (sign incy)
                   then sx!(i*incx)
                   else sx!(incx*(n-1-i))
            else sy!j
@

  The elements selected from the vector are controlled by the parameters
  n and incx.   The parameter n determines the number of elements, while
  the parameter incx determines the spacing between selected elements.

  No bound checks are performed.   The calling program should ensure that:

@
    length sx >= (1 + (n-1)incx)
    length sy >= (1 + (n-1)incy)
@
-}
scopy :: Int -> V.Vector Float -> Int -> V.Vector Float -> Int -> V.Vector Float
scopy n sx incx sy incy =
    case n<1 of
        True -> sy
        _ -> case incy of
            1  -> case incx of
                1  -> (V.unsafeTake n sx) V.++ rs
                0  -> (V.replicate n x0) V.++ rs
                -1 -> (V.reverse $ V.unsafeTake n sx) V.++ rs
                _  -> case s of
                    True -> V.unsafeUpdate_ sy ixs xs
                    _ -> V.unsafeUpdate_ sy ixs xs'
            0  -> case incx of
                0  -> V.unsafeUpd sy [(0,x0)]
                _ -> case incx>0 of
                    True -> V.unsafeUpd sy [(0,xn)]
                    _ -> V.unsafeUpd sy [(0,x0)]
            -1 -> case incx of
                -1 -> (V.unsafeTake n sx) V.++ rs
                0  -> (V.replicate n x0) V.++ rs
                1  -> (V.reverse $ V.unsafeTake n sx) V.++ rs
                _  -> case s of
                    True -> V.unsafeUpdate_ sy ixs xs
                    _ -> V.unsafeUpdate_ sy ixs xs'
            _ -> case incx of
                -1 -> if incy<0
                        then V.unsafeUpdate_ sy ixs (V.unsafeTake n sx)
                        else V.unsafeUpdate_ sy ixs (V.reverse $ V.unsafeTake n sx)
                0  -> V.unsafeUpdate_ sy ixs (V.replicate n x0)
                1  -> if incy>0
                        then V.unsafeUpdate_ sy ixs (V.unsafeTake n sx)
                        else V.unsafeUpdate_ sy ixs (V.reverse $ V.unsafeTake n sx)
                _  -> case s of
                    True -> V.unsafeUpdate_ sy ixs xs
                    _    -> V.unsafeUpdate_ sy ixs xs'
    where
    rs = V.unsafeDrop n sy
    s = (incx>0 && incy>0) || (incx<0 && incy<0)
    x0 = sx `V.unsafeIndex` 0
    xn = sx `V.unsafeIndex` ((n-1)*abs incx)
    xs = sample n sx (abs incx)
    xs' = samplerev n sx (-abs incx)
    ixs = sampleIndices n (abs incy)

copyHelper :: Int ->
   V.Vector Float -> Int -> V.Vector Float -> Int -> V.Vector Float
copyHelper n src incs dst incd =
    case compare sdsd 0 of
        -- when incx * incy is negative, then the two vectors are being traversed
        -- in opposite directions.
        LT -> case aincs of
            1 -> case aincd of
                1 -> (V.reverse ys) V.++ rs
                _ -> V.unsafeUpdate_ dst ixs' ys
            _ -> case aincd of
                1 -> (V.reverse zs) V.++ rs
                _ -> V.unsafeUpdate_ dst ixs' zs
        -- When incx * incy is zero, then at least one of the vectors is not
        -- traversed.  Either the replacement will consist of the same element
        -- being replicated into multiple destinations, or multiple elements
        -- will be (destructively) written to the same destination
        EQ -> case aincd of
            0 -> case incs > 0 of
                True  -> V.cons (V.last zs) $ V.tail dst
                False -> V.cons src0 $ V.tail dst
            1 -> V.replicate n src0 V.++ rs
            _ -> V.unsafeUpdate_ dst ixs (V.replicate n src0)
        -- when incx * incy is positive, the two vectors are being traversed
        -- in the same direction.
        GT -> case aincs of
            1 -> case aincd of
                1 -> ys V.++ rs
                _ -> V.unsafeUpdate_ dst ixs ys
            _ -> case aincd of
                1 -> zs V.++ rs
                _ -> V.unsafeUpdate_ dst ixs zs
    where
    aincs = abs incs
    aincd = abs incd
    sdsd  = (signum incs)*incd
    ixs = sampleIndices n aincd
    ixs' = sampleIndicesRev n (-aincd)
    rs = V.drop ((n-1)*aincd+1) dst
    ys = V.unsafeTake n src
    zs = sample n src aincs
    src0 = V.unsafeIndex src 0

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
                        True  -> slassq_loop (i+incx) x (1.0 + ssq * (scale/x)^(2::Int))
                        False -> slassq_loop (i+incx) scale (ssq + (x/scale)^(2::Int))
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


{- | O(1) Construct a plane Givens rotation on a deconstructed two-vector.
Specifically:

@
srotg sa sb = ( r, secant(theta), cos(theta), sin(theta))
@

where r is the signed magnitude of the vector <sa,sb>.   In the case when r
is zero, srotg returns (0,0,1,0).
-}
srotg :: Float -> Float -> GivensRot Float
srotg sa sb =
    case scale of
        0.0 -> GIVENSROT (0,0,1,0)
        _ -> case asa>asb of
            True -> let r = (signum sa) * magnitude
                        s = sb/r
                    in  GIVENSROT (r,s,sa/r,s)
            False-> let r = (signum sb) * magnitude
                        c = sa/r
                    in  case c of
                           0.0 -> GIVENSROT (r,1,0,1)
                           _   -> GIVENSROT (r,1/c,c,sb/r)
    where
        asa = abs sa
        asb = abs sb
        scale = asa + asb
        magnitude = scale * sqrt((sa/scale)^(2::Int)+(sb/scale)^(2::Int))

{- | O(1) Construct a modified plane Givens rotation on a deconstructed
two-vector as described in "Basic Linear Algebra Subprograms for Fortran
Use", Lawson 1979.  The solution should satisfy


  | d1 0 | | h11 h12 | | sx1/x1 |     | sd1 0   | | sx1 |
  | 0 d2 | | h21 h22 | | sy1/x1 | = G | 0   sd2 | | sx2 |
@

where G is the Givens rotation matirx.   d1, d2 and x1 are rescaled by the the
"checkscale" subprogram to be within the conservative limits of +/- 1/(4096)^2
and +/- 4096^2.

-}

srotmg :: Float -> Float -> Float -> Float -> ModGivensRot Float
srotmg sd1 sd2 sx1 sy1
   | sd1 < 0   = FLAGNEG1 { d1=0, d2=0, x1=0, h11=0, h12=0, h21=0, h22=0}
   | sp2 == 0  = FLAGNEG2
   | asq1>asq2 =
       let sh21 = -sy1/sx1
           sh12 = sp2/sp1
           su  = 1 - sh12*sh21
      in  if (su>0)
            then mkparms $ checkscale (0,sd1/su,sd2/su,sx1*su,0,sh12,sh21,0)
            else mkparms $ checkscale (0,sd1,sd2,sx1,0,sh12,sh21,0)
   | otherwise = if sq2 < 0
      then mkparms $ checkscale (-1,0,0,0,0,0,0,0)
      else let su = 1 + sh11*sh22
               sh11 = sp1/sp2
               sh22 = sx1/sy1
           in  mkparms $ checkscale (1,sd2/su,sd1/su,sy1*su,sh11,0,0,sh22)
   where
   sp2 = sd2*sy1
   sp1 = sd1*sx1
   sq2 = sp2*sy1
   sq1 = sp1*sx1
   asq1 = abs sq1
   asq2 = abs sq2
   {-# INLINE mkparms #-}
   mkparms :: (Int, Float, Float, Float, Float, Float, Float, Float) -> ModGivensRot Float
   mkparms (flag,d1,d2,x1,h11,h12,h21,h22) =
      case compare flag 0 of
          LT -> FLAGNEG1 { .. }
          EQ -> FLAG0 { .. }
          GT -> FLAG1 { .. }

-- | A helper function that ensures that the scale of sd1 and sd2 fall between
-- 1.677E7 and 1/1.677E7.   This is called by srotmg to scale the modified
-- Givens rotation matrix to avoid underflow.
{-# INLINE checkscale #-}
checkscale :: (Int,Float,Float,Float,Float,Float,Float,Float) -> (Int,Float,Float,Float,Float,Float,Float,Float)
checkscale = checkscale2 . checkscale1
   where
   gam, gamsq, rgamsq :: Float
   gam = 4096
   gamsq = 1.67772E7
   rgamsq = 5.96046E-8
   {-# INLINE checkscale1 #-}
   checkscale1 z@(_,sd1,_,_,_,_,_,_)
       | sd1 == 0  =z
       | otherwise = gocheck1 z
       where
       {-# INLINE gocheck1 #-}
       gocheck1 zz@(_,sd1',_,_,_,_,_,_)
           | sd1' >rgamsq && sd1' < gamsq = zz
           | otherwise = gocheck1 $ check1B $ checkA zz
       {-# INLINE check1B #-}
       check1B (f,sd1',sd2,sx1,sh11,sh12,sh21,sh22)
           | a <= rgamsq = ( f, sd1'*gam^(2::Int), sd2, sx1/gam, sh11/gam, sh12/gam, sh21, sh22)
           | otherwise   = ( f, sd1'/gam^(2::Int), sd2, sx1*gam, sh11*gam, sh12*gam, sh21, sh22)
           where a = abs sd1'
   {-# INLINE checkscale2 #-}
   checkscale2 z@(_,_,sd2,_,_,_,_,_)
       | sd2 == 0  = z
       | otherwise = gocheck2 z
       where
       {-# INLINE gocheck2 #-}
       gocheck2 zz@(_,_,sd2',_,_,_,_,_)
           | a > rgamsq && a < gamsq = zz
           | otherwise = gocheck2 $ check2B a $ checkA zz
           where a = abs sd2'
       {-# INLINE check2B #-}
       check2B a (f,sd1,sd2',sx1,sh11,sh12,sh21,sh22)
           | a <= rgamsq = ( f, sd1, sd2'*gam^(2::Int), sx1, sh11, sh12, sh21/gam, sh22/gam)
           | otherwise   = ( f, sd1, sd2'/gam^(2::Int), sx1, sh11, sh12, sh21*gam, sh22*gam)
   {-# INLINE checkA #-}
   checkA (f,sd1,sd2,sx1,sh11,sh12,sh21,sh22)
       | f==0      = ( -1, sd1, sd2, sx1, 1, sh12, sh21, 1 )
       | otherwise = ( -1, sd1, sd2, sx1, sh11, 1, -1, sh22 )
