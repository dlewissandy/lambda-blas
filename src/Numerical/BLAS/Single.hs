{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{- | This module provides BLAS level-1 library functions for vectors of
single precision floating point numbers.  In general, BLAS level-1 functions
that take vector valued arguments support the ability to apply their
functionality over all the elements in the vector, or to a smaller vector
comprised of elements drawn in uniform intervals from the original
vector argument.   This capability allows vector operations to be applied,
for example, to columns of matrices.

To support this capability, each BLAS level-1 function that has at least
one vector valued argument takes an Int parameter, n, that represents the
number of elements to be extracted from the vector arguments.  The function
also will take an additional Int argument for each vector valued argument,
representing the both the direction of traversal and the interval between
elements are drawn of the original vector argument.   Typically n is the first
Int valued argument, and the increments are the first Int valued argment
following a vector agument.

For example:

@
sdot :: Int -> Vector Float -> Int -> Vector Float -> Int -> Float
sdot n sx incx sy incy = foldl' (+) 0 $ zipWith (*) (sampleElems n sx incx) (sampleElems n sy incy)
@

is morally equivalent to the dot product of the vectors sx and sy when both
vectors have lengths of n and both incx and incy are unity.

The BLAS functions do not generally guard against invalid values of the increment
arguments.   For each vector argument v, and each increment inc, the calling
program should ensure that:

@
inc /= 0
length v >= (1 + (n-1)*inc)
@
-}
module Numerical.BLAS.Single(
   -- * Norms
   asum,sasum,dasum,
   nrm2,snrm2,dnrm2,
   -- * Products
   sdot,
   sdsdot,
   sscal,
   -- * Rotations
   srot,
   rotg,srotg,drotg,
   srotm,
   srotmg,
   -- * Linear Combinations
   saxpy,
   -- * Memory Operations
   scopy,
   sswap,
   -- * Specialized Folds
   iamax,isamax,idamax,
   ) where

import Numerical.BLAS.Types
import Numerical.BLAS.Util

import Data.Ord(comparing)
import Data.Vector.Storable(Vector)
import qualified Data.Vector.Storable as V

-- | O(n) - compute the sum of products of elements drawn from two Float vectors.
-- This function is morally equivalent to the dot product of vectors x and y
-- when n is the length of both x and y, and both incx and incy are unity.
sdot :: Int            -- ^ The number of elements n
     -> Vector Float -- ^ The first vector, x
     -> Int            -- ^ The increment, incx, between elements drawn from x
     -> Vector Float -- ^ The second vector, y
     -> Int            -- ^ The increment, incy,  between elements drawn from y
     -> Float          -- ^ The sum of products of the selected elements
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

-- | O(n) - Multiply selected elements in a vector, x, by a scalar, a.   This
-- function is morally equivalent to scalar multiplication a*x when n is the
-- length x and incx is unity.
sscal :: Int          -- ^ The number of elements to scale, n
    -> Float          -- ^ The scalar factor, a
    -> Vector Float -- ^ The vector to scale, x
    -> Int            -- ^ The increment between elements to scale, incx
    -> Vector Float -- ^ The vector with scaled elements
sscal n a sx incx =
    V.unsafeUpdate_ sx (sampleIndices n incx )
    $ V.map (*a)
    $ sampleElems n sx incx

-- | O(n) - Copy n elements from one Float vector into another.
scopy :: Int          -- ^ The number of elements to copy
    -> Vector Float -- ^ The source vector from which to copy elements
    -> Int            -- ^ The increment between elements drawn from the source vector
    -> Vector Float -- ^ The target vector into which elements are copied
    -> Int            -- ^ The increment between the destination elements
    -> Vector Float
scopy n sx incx sy incy = updateElems (\ _ x -> x ) n sy incy sx incx

-- |  O(n) -- Swap n elements between two Float vectors.
sswap :: Int          -- ^ The number of elements to copy
    -> Vector Float -- ^ The first vector from which elements will be swapped
    -> Int            -- ^ The increment between elements drawn from the first vector
    -> Vector Float -- ^ The second vector from which elements will be swapped
    -> Int            -- ^ The increment between elements drawn from the second vector
    -> (Vector Float, Vector Float)
sswap n sx incx sy incy =
    ( updateElems (\ _ y -> y ) n sx incx sy incy
    , updateElems (\ _ x -> x ) n sy incy sx incx )

-- | O(n) - asum computes the sum of the absolute value of elements drawn a
-- vector, x. This function is morally equivalent to the L1 norm
-- when n is equal to the length of x and incx is unity.
asum :: (Num a, V.Storable a)
    =>  Int          -- ^ The number of elements to sum, n
    -> Vector a    -- ^ The vector from which summands are selected, x
    -> Int           -- ^ The increment between summands, incx
    -> a
{-# INLINE asum #-}
asum n sx !incx
  | incx <1    = 0
  | otherwise  = V.foldl' (+) 0 $ V.map (abs) $ sampleElems n sx incx
{-# DEPRECATED sasum, dasum "Use asum instead" #-}
sasum :: Int -> Vector Float -> Int -> Float
sasum = asum
dasum :: Int -> Vector Double -> Int -> Double
dasum = asum


-- | O(n) - nrm2 computes the sum of the squared values of elements drawn a
-- vector, x. This function is morally equivalent to the L2 norm
-- when n is equal to the length of x and incx is unity.
nrm2 :: (Floating a, Ord a, V.Storable a)
    =>  Int          -- ^ The number of elements to sum, n
    -> Vector a    -- ^ The vector from which summands are selected, x
    -> Int           -- ^ The increment between summands, incx
    -> a
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
{-# DEPRECATED snrm2, dnrm2 "Use nrm2 instead" #-}
snrm2 :: Int-> Vector Float-> Int-> Float
snrm2 = nrm2
dnrm2 :: Int-> Vector Double-> Int-> Double
dnrm2 = nrm2


-- | O(n) - Find the index of the element with the largest absolute value
-- from elements drawn from a vector.   This function is morally
-- equivalent to the index of the element with the largest magnitude when
-- n is the length of the vector x and the increment, inc, is unity.
iamax :: (Ord a, Num a, V.Storable a)
    => Int         -- ^ The number of elements drawn from the source vector,n
    -> Vector a  -- ^ The source vector, x
    -> Int         -- ^ The interval between elements drawn from the source vector, incx
    -> Int         -- ^ The index of the element with the largest absolute value.
{-# INLINE idamax #-}
iamax !n sx !incx
   | n < 1 || incx < 1 = -1
   | n == 1            = 0
   | otherwise         = V.maxIndexBy (comparing abs) $ sampleElems n sx incx
{-# DEPRECATED isamax,idamax "use iamax instead" #-}
isamax :: Int-> Vector Float-> Int-> Int
isamax = iamax
idamax :: Int-> Vector Double-> Int-> Int
idamax = iamax

-- | O(n) - compute the sum of products of elements drawn from two Float vectors
-- using Double precision internally. This function is morally equivalent to the
-- dot product of vectors x and y when n is the length of both x and y, and both
-- incx and incy are unity.
sdsdot :: Int            -- ^ The number of elements n
     -> Float          -- ^ a Vector valued adjustment to add to the sum
     -> Vector Float -- ^ The first vector, x
     -> Int            -- ^ The increment, incx, between elements drawn from x
     -> Vector Float -- ^ The second vector, y
     -> Int            -- ^ The increment, incy,  between elements drawn from y
     -> Float          -- ^ The sum of products of the selected elements
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
rotg sa sb = ( r, secant(theta), cos(theta), sin(theta))
@

where r is the signed magnitude of the vector <sa,sb>.   In the case when r
is zero, rotg returns (0,0,1,0).   See also "rot".
-}
rotg :: (Fractional a, Floating a, Ord a) 
   => a    -- ^ The x coordinate of the deconstructed vector
   -> a    -- ^ The y coordinate of the deconstructed vector
   -> GivensRot a -- ^ The Givens rotation coefficients
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
{-# DEPRECATED srotg, drotg "Use rotg instead." #-}
srotg :: Float -> Float -> GivensRot Float
srotg = rotg
drotg :: Double -> Double -> GivensRot Double
drotg = rotg


{- | O(1) Construct a modified plane Givens rotation on a deconstructed
two-vector as described in "Basic Linear Algebra Subprograms for Fortran
Use", Lawson 1979.  The solution should satisfy


  | d1 0 | | h11 h12 | | sx1/x1 |     | sd1 0   | | sx1 |
  | 0 d2 | | h21 h22 | | sy1/x1 | = G | 0   sd2 | | sx2 |
@

where G is the Givens rotation matirx.   d1, d2 and x1 are rescaled by the the
"checkscale" subprogram to be within the conservative limits of +/- 1/(4096)^2
and +/- 4096^2.

See also "rotm".
-}
srotmg :: Float -- ^ The first diagonal element sd1
    -> Float    -- ^ The second diagonal element sd2
    -> Float    -- ^ The first component of the vector sx1
    -> Float    -- ^ The second component of the vector sx2
    -> ModGivensRot Float -- ^ The modified Givens Rotation coefficients
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


-- | O(n) apply a Givens rotation to elements drawn from a pair of vectors.
-- This is morally equivalent to rotating both vectors through an angle when
-- n is equal to the length of both the vectors, and both incx and incy are
-- unity.
srot :: Int          -- ^ The number of elements to rotate
    -> Vector Float  -- ^ The first vector to rotate, x
    -> Int           -- ^ The interval between rotated elements in the first vector, incx
    -> Vector Float  -- ^ The second vector to rotate, y
    -> Int           -- ^ The interval between rotated elements in the second vector, incy
    -> Float         -- ^ The cosine of the angle of rotation
    -> Float         -- ^ The sine of the angle of rotation
    -> (Vector Float, Vector Float) -- the rotated vectors
srot n u incx v incy c s =
    ( updateElems (\ x y -> c*x + s*y) n u incx v incy
    , updateElems (\ y x -> c*y - s*x) n v incy u incx )


{- | O(n) compute the linear combination of elements drawn from two vectors
according.  This is morally equivalent to the linear combination of the two
vectors when n is the length of the vectors and both incx and incy are unity.
-}
saxpy :: Int        -- ^ The number of elements
    -> Float        -- ^ The scalar multiple for the first vector, a
    -> Vector Float -- ^ The first vector, x
    -> Int          -- ^ The interval between elements drawn from the first vector, incx
    -> Vector Float -- ^ The second vector, y
    -> Int          -- ^ The interval between elements drawn from the second vector, incy
    -> Vector Float -- ^ The linear combination a*x + y
saxpy n a sx incx sy incy = updateElems (\ y x -> y + a*x ) n sy incy sx incx

{- | O(n) apply a modified Givens rotation to elements drawn from a pair of
vectors according to the following specification:

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
see also "rotmg".
-}
srotm :: ModGivensRot Float
    -> Int
    -> Vector Float
    -> Int
    -> Vector Float
    -> Int
    -> ( Vector Float, Vector Float )
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
