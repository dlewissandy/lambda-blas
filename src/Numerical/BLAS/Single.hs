{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
-- | This module provides BLAS library functions for vectors of
-- single precision floating point numbers.
module Numerical.BLAS.Single(
   isamax,idamax,
   sdot,
   sasum,dasum,
   snrm2,dnrm2,
   sdsdot,
   srot,
   rotg,srotg,drotg,
   srotm,
   srotmg,
   sscal,
   scopy,
   sswap,
   saxpy,
    ) where

import Numerical.BLAS.Types
import Numerical.BLAS.Util

import Data.Ord(comparing)
import Data.Vector.Storable(Vector)
import qualified Data.Vector.Storable as V

{- | O(n) sdot function computes the sum of the products of elements
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

-}
sdot :: Int -> V.Vector Float -> Int -> V.Vector Float -> Int -> Float
sdot !n sx !incx sy !incy = V.foldl' (+) 0
    $ V.zipWith (*) (sampleElems n sx incx) $ sampleElems n sy incy


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
sscal n a sx incx =
    V.unsafeUpdate_ sx (sampleIndices n incx )
    $ V.map (*a)
    $ sampleElems n sx incx

{- | O(n) copy n elements from one vector into another according to the
following specification

@
   forall incx>0, n>0, 0<=j<=length v 0<=i<n
     (scopy n sx incx sy incy)!i =
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
scopy n sx incx sy incy = updateElems (\ _ x -> x ) n sy incy sx incx

{- | O(n) swap n elements between two vectors according to the following
specification:

@
   forall incx/=0, incy/=0, n>0
     sswap n u incx v incy = (scopy n v incy u incx, scopy n u incx v incy)
@

  The elements selected from the vector are controlled by the parameters
  n and incx.   The parameter n determines the number of elements, while
  the parameter incx determines the spacing between selected elements.

  No bound checks are performed.   The calling program should ensure that:

@
    length sx >= (1 + (n-1)abs incx)
    length sy >= (1 + (n-1)abs incy)
@
-}
sswap :: Int -> V.Vector Float -> Int -> V.Vector Float -> Int -> (V.Vector Float, V.Vector Float)
sswap n sx incx sy incy =
    ( updateElems (\ _ y -> y ) n sx incx sy incy
    , updateElems (\ _ x -> x ) n sy incy sx incx )

{- | O(n) sasum computes the sum of the absolute value of elements drawn a
  vector according to the following specification

@
   sasum n u incx = sum $ map abs $ sampleElems n u incx
@

  The elements selected from the vector are controlled by the parameters
  n and incx.   The parameter n determines the number of summands, while
  the parameter incx determines the spacing between selected elements.

  No bound checks are performed.   The calling program should ensure that:

@
    length u >= (1 + (n-1)*abs(incx))
@
-}
sasum :: Int -> V.Vector Float -> Int -> Float
sasum = asum
{- | O(n) dasum computes the sum of the absolute value of elements drawn a
  vector according to the following specification

@
   dasum n u incx = sum $ map abs $ sampleElems n u incx
@

  The elements selected from the vector are controlled by the parameters
  n and incx.   The parameter n determines the number of summands, while
  the parameter incx determines the spacing between selected elements.

  No bound checks are performed.   The calling program should ensure that:

@
    length u >= (1 + (n-1)*abs(incx))
@
-}
dasum :: Int -> V.Vector Double -> Int -> Double
dasum = asum
-- A helper function for computing the L1 Norm
asum :: (Num a, V.Storable a) => Int -> Vector a -> Int -> a
{-# INLINE asum #-}
asum n sx !incx
  | incx <1    = 0
  | otherwise  = V.foldl' (+) 0 $ V.map (abs) $ sampleElems n sx incx

{- | O(n) snrm2 computes the sum of the squares of elements drawn a
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
snrm2 :: Int -> Vector Float -> Int -> Float
snrm2 = nrm2
{- | O(n) dnrm2 computes the sum of the squares of elements drawn a
vector according to the following specification

@
 dnrm2 n u incx = sum { u[i*incx] ^2   | i<=[0..n-1] }
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
dnrm2 :: Int -> Vector Double -> Int -> Double
dnrm2 = nrm2
nrm2 :: (Floating a, Ord a, V.Storable a)
 => Int -- ^ The number of summands
 -> Vector a -- ^ the vector u
 -> Int          -- ^ the space between elements drawn from u
 -> a        -- ^ The l2 norm of the vector u
{-# INLINE nrm2 #-}
nrm2 !n sx !incx
    | n<1 || incx<1 = 0.0
    | n==1          = abs $ sx `V.unsafeIndex` 0
    | otherwise     = slassq (incx*(n-1))
    where
    slassq !imax = slassq_loop 0 0.0 1.0
        where
        {-# INLINE slassq_loop #-}
        slassq_loop !i !scale !ssq
            | i > imax = scale*sqrt ssq
            | otherwise =
                let xi = sx `V.unsafeIndex` i
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
    isamax n u incx = maxIndexBy ( comparing abs ) $ sampleElems n u incx
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
isamax = iamax
{- | O(n) idamax computes the index of the element of a vector having the
largest absolute value according to the following specification.

@
    idamax n u incx = maxIndexBy ( comparing abs ) $ sampleElems n u incx
@

 The elements selected from the vector are controlled by the parameters
 n and incx.   The parameter n determines the number of summands, while
 the parameter incx determines the spacing between selected elements.

 No bound checks are performed.   The calling program should ensure that:

@
   length u >= (1 + (n-1)*abs(incx))
@
-}
idamax :: Int -> Vector Double -> Int -> Int
idamax = iamax
iamax :: (Ord a, Num a, V.Storable a)
    => Int -> V.Vector a -> Int -> Int
{-# INLINE idamax #-}
iamax !n sx !incx
   | n < 1 || incx < 1 = -1
   | n == 1            = 0
   | otherwise         = V.maxIndexBy (comparing abs) $ sampleElems n sx incx

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


{- | O(1) Construct a plane Givens rotation on a deconstructed two-vector.
Specifically:

@
srotg sa sb = ( r, secant(theta), cos(theta), sin(theta))
@

where r is the signed magnitude of the vector <sa,sb>.   In the case when r
is zero, srotg returns (0,0,1,0).
-}
srotg :: Float -> Float -> GivensRot Float
srotg = rotg
{- | O(1) Construct a plane Givens rotation on a deconstructed two-vector.
Specifically:

@
drotg sa sb = ( r, secant(theta), cos(theta), sin(theta))
@

where r is the signed magnitude of the vector <sa,sb>.   In the case when r
is zero, drotg returns (0,0,1,0).
-}
drotg :: Double -> Double -> GivensRot Double
drotg = rotg
{- | O(1) Construct a plane Givens rotation on a deconstructed two-vector.
Specifically:

@
rotg sa sb = ( r, secant(theta), cos(theta), sin(theta))
@

where r is the signed magnitude of the vector <sa,sb>.   In the case when r
is zero, rotg returns (0,0,1,0).
-}
rotg :: (Fractional a, Floating a, Ord a) => a -> a -> GivensRot a
{-# INLINE rotg #-}
rotg !sa !sb =
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


{- | O(n) apply a Givens rotation to a pair of vectors

  The elements selected from the vector are controlled by the parameters
  n and incx.   The parameter n determines the number of elements, while
  the parameter incx determines the spacing between selected elements.

  No bound checks are performed.   The calling program should ensure that:

@
    length sx >= (1 + (n-1)abs incx)
    length sy >= (1 + (n-1)abs incy)
@
-}
srot :: Int -> V.Vector Float -> Int -> V.Vector Float -> Int -> Float -> Float -> (V.Vector Float, V.Vector Float)
srot n u incx v incy c s =
    ( updateElems (\ x y -> c*x + s*y) n u incx v incy
    , updateElems (\ y x -> c*y - s*x) n v incy u incx )


{- | O(n) compute the linear combination of elements drawn from two vectors
according to the following rule:

saxpy n a sx incx sy incy = updateElems (\ x y -> y + a*x) n sy incy sx incx

The elements selected from the vector are controlled by the parameters
n and incx.   The parameter n determines the number of elements, while
the parameter incx determines the spacing between selected elements.

No bound checks are performed.   The calling program should ensure that:

@
    length sx >= (1 + (n-1)abs incx)
    length sy >= (1 + (n-1)abs incy)
@
-}
saxpy :: Int -> Float -> V.Vector Float -> Int -> V.Vector Float -> Int -> V.Vector Float
saxpy n a sx incx sy incy = updateElems (\ y x -> y + a*x ) n sy incy sx incx

{- | O(n) apply a modified Givens rotation to a pair of vectors according to
the following specification:

@
srotm FLAGNEG2 n sx incx sy incy = (sx,sy)
srotm (FLAGNEG1 h11 h12 h21 h22) n sx incx sy incy =
   ( updateElems (\ x y -> h11*x + h12*y) n sx incx sy incy
   , updateElems (\ y x -> h21*x + h22*y) n sy incy sx incx)
srotm (FLAG0 h12 h21) n sx incx sy incy =
   ( updateElems (\ x y -> x + h12*y) n sx incx sy incy
   , updateElems (\ y x -> h21*x + y) n sy incy sx incx)
srotm (FLAG1 h11 h22) n sx incx sy incy =
   ( updateElems (\ x y -> h11*x + y) n sx incx sy incy
   , updateElems (\ y x -> -x + h22*y) n sy incy sx incx)
@
The elements selected from the vector are controlled by the parameters
n and incx.   The parameter n determines the number of elements, while
the parameter incx determines the spacing between selected elements.

No bound checks are performed.   The calling program should ensure that:

@
    length sx >= (1 + (n-1)abs incx)
    length sy >= (1 + (n-1)abs incy)
@
-}
srotm :: ModGivensRot Float
    -> Int
    -> V.Vector Float
    -> Int
    -> V.Vector Float
    -> Int
    -> ( V.Vector Float, V.Vector Float )
{-# INLINE srotm #-}
srotm flag !n sx !incx sy !incy = case flag of
    FLAGNEG2 -> (sx,sy)
    FLAGNEG1 _ _ _ sh11 sh12 sh21 sh22 ->
        ( updateElems (\ x y -> sh11*x + sh12*y) n sx incx sy incy
        , updateElems (\ y x -> sh21*x + sh22*y) n sy incy sx incx)
    FLAG0 _ _ _ sh12 sh21 ->
        ( updateElems (\ x y -> x + sh12*y) n sx incx sy incy
        , updateElems (\ y x -> sh21*x + y) n sy incy sx incx)
    FLAG1 _ _ _ sh11 sh22 ->
        ( updateElems (\ x y -> sh11*x + y) n sx incx sy incy
        , updateElems (\ y x -> -x + sh22*y) n sy incy sx incx)
