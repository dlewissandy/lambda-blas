{-# LANGUAGE BangPatterns #-}
-- | This module contains the benchmark test to compare the time
-- performance of the Naive, Native Haskell and CBLAS implementations of
-- the BLAS subroutines
module Main( main ) where

import qualified Data.Vector.Storable as V
import Foreign.Marshal.Array
import Test.QuickCheck.Gen
import Foreign.Ptr
import Criterion.Main
-- | This package
import Numerical.BLAS.Single
-- | This test suite
import Gen
import qualified Foreign as FORTRAN

main :: IO ()
main = do
    let n = 10001
    u<- generate $ genNVector (genFloat genEveryday) n
    v<- generate $ genNVector (genFloat genEveryday) n
    withArray (V.toList u) $ \ us ->
       withArray (V.toList v) $ \ vs ->
        defaultMain [ level1_benchs n u v us vs ]

-- | Level 1 BLAS benchmarks
level1_benchs :: Int -> V.Vector Float -> V.Vector Float
           -> Ptr Float -> Ptr Float  -> Benchmark
level1_benchs n u v us vs = bgroup "level-1"
    [ sdot_benchs n u v us vs
    , sasum_benchs n u us
    ]

-- Benchmarks for the sdot function
sdot_benchs :: Int -> V.Vector Float -> V.Vector Float
    -> Ptr Float -> Ptr Float -> Benchmark
sdot_benchs n u v us vs = bgroup "sdot"
   [ bench "sdot_zip(u,v)" $ nf naive (u,v)
   , native 1 1          -- test corner case when both incx and incy are 1
   , foreign 1 1
   , native (-1) (-1)    -- test corner case when incx or incy is not equal to 1
   , foreign (-1) (-1)
   ]
   where
   naive (a,b) = sdot_zip a b
   native !incx !incy = bench ("sdot(n,u,"++show incx++",v,"++show incy++")") $
       nf (\ (a,b,c,d,e) -> sdot a b c d e) (n,u,incx,v,incy)
   foreign !incx !incy = bench ("FOREIGN.sdot(n,u,"++show incx++",v,"++show incy++")") $
       nfIO $ FORTRAN.sdot n us incx vs incy

-- | benchmarks for the sasum function
sasum_benchs :: Int -> V.Vector Float -> Ptr Float -> Benchmark
sasum_benchs n u us = bgroup "sasum"
  [ native 0    -- test corner case when incx < 1
  , foreign 0
  , native 1    -- test corner case when incx = 1
  , foreign 1
  , native 2    -- test corner case when incx >1
  , foreign 2
  ]
  where
  native !incx = bench ("sasum(n,u,"++show incx++")") $
      nf (\ (a,b,c) -> sasum a b c) (n,u,incx)
  foreign !incx = bench ("FOREIGN.sasum(n,u,"++show incx++")") $
      nfIO $ FORTRAN.sasum n us incx
