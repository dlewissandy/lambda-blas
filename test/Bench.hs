{-# LANGUAGE BangPatterns #-}
-- | This module contains the benchmark test to compare the time
-- performance of the Naive, Native Haskell and CBLAS implementations of
-- the BLAS subroutines
module Main( main ) where

import qualified Data.Vector.Storable as V
import Test.QuickCheck.Gen
import Criterion.Main
import Control.DeepSeq
-- | This package
import Numerical.BLAS.Single
-- | This test suite
import Gen
import qualified Foreign as F

main :: IO ()
main = do
    -- GENERATE RANDOM DATA ON WHICH TO EVALUATE THE BENCHMARKS
    sx<- generate $ genNVector (genFloat genEveryday) nmax
    sy<- generate $ genNVector (genFloat genEveryday) nmax
    dx<- generate $ genNVector (genDouble genEveryday) nmax
    dy<- generate $ genNVector (genDouble genEveryday) nmax
    sa<- generate $ genFloat genEveryday
    sb<- generate $ genFloat genEveryday
    sc<- generate $ genFloat genEveryday
    sd<- generate $ genFloat genEveryday
    da<- generate $ genDouble genEveryday
    db<- generate $ genDouble genEveryday
    dc<- generate $ genDouble genEveryday
    dd<- generate $ genDouble genEveryday
    -- DEFINE THE WRAPPER FUNCTIONS USED TO APPLY THE RANDOM VALUES
    let dotHelper  f x y !n !inc = f n x inc y inc
        normHelper f x !n !inc = f n x inc
        scalHelper f !a x !n !inc = f n a (trim n x inc) inc
        axpyHelper f !a x y !n !inc = f n a (trim n x inc) inc (trim n y inc) inc
        copyHelper f x y !n !inc = f n (trim n x inc) inc (trim n y inc) inc
        rotHelper f !a !b x y !n !inc = f n (trim n x inc) inc (trim n y inc) inc a b
        rotmHelper f !z x y !n !inc = f z n x inc y inc
        trim n x inc = V.unsafeTake (1+(n-1)*inc) x
        flags = rotmg sa sb sc sd
        dflags = rotmg da db dc dd
    -- RUN THE BENCHMARKS
    defaultMain [  bgroup "level-1" [
        vectorbench "sdot"   (dotHelper dot sx sy)
                             (dotHelper F.sdot_unsafe sx sy)
                             (dotHelper F.sdot sx sy),
        vectorbench "ddot"   (dotHelper dot dx dy)
                             (dotHelper F.ddot_unsafe dx dy)
                             (dotHelper F.ddot dx dy),
        vectorbench "sasum"  (normHelper asum sx)
                             (normHelper F.sasum_unsafe sx)
                             (normHelper F.sasum sx),
        vectorbench "snrm2"  (normHelper nrm2 sx)
                             (normHelper F.snrm2_unsafe sx)
                             (normHelper F.snrm2 sx),
        vectorbench "isamax" (normHelper iamax sx)
                             (normHelper F.isamax_unsafe sx)
                             (normHelper F.isamax sx),
        vectorbench "dasum"  (normHelper asum dx)
                             (normHelper F.dasum_unsafe dx)
                             (normHelper F.dasum dx),
        vectorbench "dnrm2"  (normHelper nrm2 dx)
                             (normHelper F.dnrm2_unsafe dx)
                             (normHelper F.dnrm2 dx),
        vectorbench "idamax" (normHelper iamax dx)
                             (normHelper F.idamax_unsafe dx)
                             (normHelper F.idamax dx),
        vectorbench "sdsdot" (axpyHelper sdsdot sa sx sy)
                             (axpyHelper F.sdsdot_unsafe sa sx sy)
                             (axpyHelper F.sdsdot sa sx sy),
        vectorbench "saxpy"  (axpyHelper axpy sa sx sy)
                             (axpyHelper F.saxpy_unsafe sa sx sy)
                             (axpyHelper F.saxpy sa sx sy),
        vectorbench "daxpy"  (axpyHelper axpy da dx dy)
                             (axpyHelper F.daxpy_unsafe da dx dy)
                             (axpyHelper F.daxpy da dx dy),
        scalarbench2 "srotg" rotg F.srotg_unsafe F.srotg sa sb,
        scalarbench2 "drotg" rotg F.drotg_unsafe F.drotg da db,
        scalarbench4 "srotmg" rotmg F.srotmg_unsafe F.srotmg sa sb sc sd,
        scalarbench4 "drotmg" rotmg F.drotmg_unsafe F.drotmg da db dc dd,
        vectorbench "sscal"  (scalHelper scal sa sx)
                             (scalHelper F.sscal_unsafe sa sx)
                             (scalHelper F.sscal sa sx),
        vectorbench "dscal"  (scalHelper scal da dx)
                             (scalHelper F.dscal_unsafe da dx)
                             (scalHelper F.dscal da dx),
        vectorbench "scopy"  (copyHelper copy sx sy)
                             (copyHelper F.scopy_unsafe sx sy)
                             (copyHelper F.scopy sx sy),
        vectorbench "dcopy"  (copyHelper copy dx dy)
                             (copyHelper F.dcopy_unsafe dx dy)
                             (copyHelper F.dcopy dx dy),
        vectorbench "sswap"  (copyHelper swap sx sy)
                             (copyHelper F.sswap_unsafe sx sy)
                             (copyHelper F.sswap sx sy),
        vectorbench "dswap"  (copyHelper swap dx dy)
                             (copyHelper F.dswap_unsafe dx dy)
                             (copyHelper F.dswap dx dy),
        vectorbench "srot"   (rotHelper rot sa sb sx sy)
                             (rotHelper F.srot_unsafe sa sb sx sy)
                             (rotHelper F.srot sa sb sx sy),
        vectorbench "drot"   (rotHelper rot da db dx dy)
                             (rotHelper F.drot_unsafe da db dx dy)
                             (rotHelper F.drot da db dx dy),
        vectorbench "srotm"  (rotmHelper rotm flags sx sy)
                             (rotmHelper F.srotm_unsafe flags sx sy)
                             (rotmHelper F.srotm flags sx sy),
        vectorbench "drotm"  (rotmHelper rotm dflags dx dy)
                             (rotmHelper F.drotm_unsafe dflags dx dy)
                             (rotmHelper F.drotm dflags dx dy)
        ]]
    where nmax = 32767

-- | benchmarks for blas functions
vectorbench :: (NFData a)
    => String                 -- the name of the test group
    -> ( Int -> Int -> a )    -- the pure version of the function
    -> ( Int -> Int -> IO a)  -- the unsafe foreign version of the function
    -> ( Int -> Int -> IO a)  -- the safe foreign version of the function
    -> Benchmark
vectorbench testname func unsafe safe = bgroup testname
  [ bgroup "pure"   [ benchPure func c | c <-cs]
  , bgroup "unsafe"   [ benchIO unsafe n inc | (n,inc)<-cs]
  , bgroup "safe"     [ benchIO safe n inc  | (n,inc)<-cs]
  ]
  where
  lengths = let zs = [1..9]++map (*10) zs in takeWhile (<32767) zs
  cs = [ (n,inc) | inc<-[1,10,100],n<-lengths,(n-1)*inc<32767]
  benchPure f c@(!n,!inc) = bench (showTestCase n inc) $ nf (uncurry f) c
  benchIO f !n !inc = bench (showTestCase n inc) $ nfIO $ f n inc
  showTestCase n inc = testname++"("++show n++","++show inc++")"

-- | benchmarks for blas functions that take two scalar arguments
scalarbench2 :: (NFData b)
    => String                 -- the name of the test group
    -> ( a -> a -> b )    -- the pure version of the function
    -> ( a -> a -> IO b)  -- the unsafe foreign version of the function
    -> ( a -> a -> IO b)  -- the safe foreign version of the function
    -> a
    -> a
    -> Benchmark
scalarbench2 testname func unsafe safe sa sb = bgroup testname
  [ bench "pure" $ nf (uncurry func) (sa,sb)
  , bench "unsafe" $ nfIO $ unsafe sa sb
  , bench "safe"   $ nfIO $ safe sa sb
  ]

-- | benchmarks for blas functions that take four scalar arguments
scalarbench4 :: (NFData b)
    => String                 -- the name of the test group
    -> ( a -> a -> a -> a -> b )    -- the pure version of the function
    -> ( a -> a -> a -> a -> IO b)  -- the unsafe foreign version of the function
    -> ( a -> a -> a -> a -> IO b)  -- the safe foreign version of the function
    -> a -> a -> a -> a
    -> Benchmark
scalarbench4 testname func unsafe safe sa sb sc sd = bgroup testname
  [ bench "pure" $ nf (\(a,b,c,d) -> func a b c d) (sa,sb,sc,sd)
  , bench "unsafe" $ nfIO $ unsafe sa sb sc sd
  , bench "safe"   $ nfIO $ safe sa sb sc sd
  ]
