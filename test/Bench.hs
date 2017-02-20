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
    let n = 32767
    u<- generate $ genNVector (genFloat genEveryday) n
    v<- generate $ genNVector (genFloat genEveryday) n
    let xs = V.toList u
        ys = V.toList v
    a<- generate $ genFloat genEveryday
    withArray xs $ \ us ->
       withArray ys $ \ vs ->
        defaultMain [ level1_benchs n a u v us vs xs ys ]

-- | Level 1 BLAS benchmarks
level1_benchs :: Int -> Float -> V.Vector Float -> V.Vector Float
           -> Ptr Float -> Ptr Float -> [Float] -> [Float] -> Benchmark
level1_benchs n a u v us vs xs ys = bgroup "level-1"
    [ sdot_benchs n u v us vs xs ys
    , sasum_benchs n u us
    , snrm2_benchs n u us
    , isamax_benchs n u us
    , sdsdot_benchs n a u v us vs
    ]

-- Benchmarks for the sdot function
sdot_benchs :: Int -> V.Vector Float -> V.Vector Float
    -> Ptr Float -> Ptr Float -> [Float] -> [Float] -> Benchmark
sdot_benchs nmax u v us vs xs ys = bgroup "sdot"
   [ bgroup "list"   [ benchPure sdot_list n xs ys inc | inc<-[1,-1], n<-ns]
   , bgroup "list"   [ benchPure sdot_list n xs ys inc | inc<-[-100..100], n<-[100]]
   , bgroup "stream" [ benchPure sdot_stream n u v inc | inc<-[1,-1], n<-ns]
   , bgroup "sdot"   [ benchPure sdot n u v inc | inc<-[1,-1], n<-ns]
   , bgroup "unsafe" [ benchIO FORTRAN.sdot_unsafe n inc | inc<-[1,-1], n<-ns]
   , bgroup "safe"   [ benchIO FORTRAN.sdot n inc | inc<-[1,-1], n<-ns]
   ]
   where
   ns = let zs = [1..9]++map (*10) zs in takeWhile (<nmax) zs
   benchPure f !n x y !inc = bench (showTestCase n inc) $
       nf (\ (a,b,c,d,e) -> f a b c d e ) (n,x,inc,y,inc)
   benchIO f !n !inc = bench (showTestCase n inc) $
       nfIO $ f n us inc vs inc
   showTestCase n inc = "sdot("++show n++",u,"++show inc++",v,"++show inc++")"

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

-- | benchmarks for the snrm2 function
snrm2_benchs :: Int -> V.Vector Float -> Ptr Float -> Benchmark
snrm2_benchs n u us = bgroup "snrm2"
    [ native 0    -- test corner case when incx < 1
    , foreign 0
    , native 1    -- test corner case when incx = 1
    , foreign 1
    , native 2    -- test corner case when incx >1
    , foreign 2
    ]
    where
    native !incx = bench ("srnm2(n,u,"++show incx++")") $
        nf (\ (a,b,c) -> snrm2 a b c) (n,u,incx)
    foreign !incx = bench ("FOREIGN.snrm2(n,u,"++show incx++")") $
        nfIO $ FORTRAN.snrm2 n us incx

-- | benchmarks for the isamax function
isamax_benchs :: Int -> V.Vector Float -> Ptr Float -> Benchmark
isamax_benchs n u us = bgroup "isamax"
    [ native 0    -- test corner case when incx < 1
    , foreign 0
    , native 1    -- test corner case when incx = 1
    , foreign 1
    , native 2    -- test corner case when incx >1
    , foreign 2
    ]
    where
    native !incx = bench ("isamax(n,u,"++show incx++")") $
        nf (\ (a,b,c) -> isamax a b c) (n,u,incx)
    foreign !incx = bench ("FOREIGN.isamax(n,u,"++show incx++")") $
        nfIO $ FORTRAN.isamax n us incx

-- Benchmarks for the sdsdot function
sdsdot_benchs :: Int -> Float -> V.Vector Float -> V.Vector Float
    -> Ptr Float -> Ptr Float -> Benchmark
sdsdot_benchs n a u v us vs = bgroup "sdsdot"
   [ native 1 1          -- test corner case when both incx and incy are 1
   , foreign 1 1
   , native (-1) (-1)    -- test corner case when incx and incy are not equal or are negative
   , foreign (-1) (-1)
   ]
   where
   native !incx !incy = bench ("sdsdot(n,aa,u,"++show incx++",v,"++show incy++")") $
       nf (\ (aa,b,c,d,e,f) -> sdsdot aa b c d e f) (n,a,u,incx,v,incy)
   foreign !incx !incy = bench ("FOREIGN.sdsdot(n,a,u,"++show incx++",v,"++show incy++")") $
       nfIO $ FORTRAN.sdsdot n a us incx vs incy
