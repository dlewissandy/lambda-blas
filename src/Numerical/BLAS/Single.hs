-- | This module provides BLAS library functions for vectors of
-- single precision floating point numbers.
module Numerical.BLAS.Single(
   sdot_zip,
   sdot,
    ) where

import Data.Vector.Storable(Vector)
import qualified Data.Vector.Storable as V

{- | O(n) compute the dot product of two vectors using zip and fold
-}
sdot_zip :: Vector Float -- ^ The vector u
    -> Vector Float      -- ^ The vector v
    -> Float             -- ^ The dot product u . v
sdot_zip u v = V.foldr (+) 0 $ V.zipWith (*) u v


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
   | incx /=1 || incy /= 1 = V.foldl1' (+) . V.generate n $ productElems (ithIndex incx) (ithIndex incy)
   | n < 5     = V.foldl1' (+) . V.zipWith (*) sx $ sy
   | m == 0    = hyloL splitAndMul5 sum5 0 (sx,sy)
   | otherwise =
        let (lx,rx) = V.splitAt m sx
            (ly,ry) = V.splitAt m sy
            subtotal = V.foldl1' (+) . V.zipWith (*) lx $ ly
        in  hyloL splitAndMul5 sum5 subtotal (rx, ry)
    where
        m :: Int
        m = n `mod` 5
        productElems :: (Int->Int) -> (Int->Int) -> Int -> Float
        productElems indexX indexY i = sx `V.unsafeIndex` (indexX i)
                        * sy `V.unsafeIndex` (indexY i)
        splitAndMul5 :: (V.Vector Float, V.Vector Float) -> Maybe (V.Vector Float, (V.Vector Float, V.Vector Float))
        splitAndMul5 (xs,ys)
             | V.null xs || V.null ys = Nothing
             | otherwise              = Just (
                 V.zipWith (*) (V.take 5 xs) (V.take 5 ys)
                 , (V.drop 5 xs, V.drop 5 ys))
        ithIndex :: Int -> Int -> Int
        {-# INLINE [0] ithIndex #-}
        ithIndex inc
            | inc>0     = \ i -> inc*i
            | otherwise = \ i -> (1+i-n)*inc
        sum5 :: Float -> V.Vector Float -> Float
        sum5 subtotal' summands = subtotal' + summands `V.unsafeIndex` 0
            + summands `V.unsafeIndex` 1 + summands `V.unsafeIndex` 2
            + summands `V.unsafeIndex` 3 + summands `V.unsafeIndex` 4

hyloL :: ( a -> Maybe (b,a)) -> (c -> b -> c) -> c -> a -> c
{-# INLINE [1] hyloL #-}
hyloL f g = hylo_loop
   where
   {-# INLINE [0] hylo_loop #-}
   hylo_loop c a = do
       let r = f a
       case r of
           Nothing -> c
           Just (b,a') -> hylo_loop (g c b) a'
