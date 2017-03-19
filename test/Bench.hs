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
    , srotg_benchs (V.head u) (V.head v)
    , srotmg_benchs a (V.unsafeIndex u 0) (V.unsafeIndex u 1) (V.unsafeIndex u 2)
    , sscal_benchs n a u
    , scopy_benchs n u v
    , sswap_benchs n u v
    , srot_benchs n u v (cos a) (sin a)
    , saxpy_benchs n a u v
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
sasum_benchs nmax u us = bgroup "sasum"
  [ bgroup "stream"   [ benchPure sasum n u inc | inc<-[1,-1], n<-ns]
  , bgroup "unsafe"   [ benchIO FORTRAN.sasum_unsafe n inc | inc<-[1,-1], n<-ns]
  , bgroup "safe"     [ benchIO FORTRAN.sasum n inc | inc<-[1,-1], n<-ns]
  ]
  where
   ns = let zs = [1..9]++map (*10) zs in takeWhile (<nmax) zs
   benchPure f !n x !inc = bench (showTestCase n inc) $
       nf (\ (a,b,c) -> f a b c ) (n,x,inc)
   benchIO f !n !inc = bench (showTestCase n inc) $
       nfIO $ f n us inc
   showTestCase n inc = "sasum("++show n++",u,"++show inc++")"

-- | benchmarks for the sscal function
sscal_benchs :: Int -> Float -> V.Vector Float -> Benchmark
sscal_benchs nmax !a u = bgroup "sscal"
  [ bgroup "stream"   [ benchPure sscal n inc | (n,inc)<-cs]
  , bgroup "unsafe"   [ benchIO FORTRAN.sscal_unsafe n inc | (n,inc)<-cs]
  , bgroup "safe"     [ benchIO FORTRAN.sscal n inc | (n,inc)<-cs]
  ]
  where
   ns = let zs = [1..9]++map (*10) zs in takeWhile (<nmax) zs
   cs = [ (n,inc) | inc<-[1,10,100],n<-ns,(n-1)*inc<nmax]
   benchPure f !n !inc = bench (showTestCase n inc) $
          nf (\ (aa,b,c,d) -> f aa b c d) (n,a,V.take (1+(n-1)*inc) u,inc)
   benchIO f !n !inc = bench (showTestCase n inc) $
       nfIO $ f n a (V.take (1+(n-1)*inc) u) inc
   showTestCase n inc = "sscal("++show n++",a,u,"++show inc++")"

-- | benchmarks for the scopy function
scopy_benchs :: Int -> V.Vector Float -> V.Vector Float -> Benchmark
scopy_benchs nmax u v = bgroup "scopy"
  [ bgroup "stream"   [ benchPure scopy n inc | (n,inc)<-cs]
  , bgroup "unsafe"   [ benchIO FORTRAN.scopy_unsafe n inc | (n,inc)<-cs]
  , bgroup "safe"     [ benchIO FORTRAN.scopy n inc | (n,inc)<-cs]
  ]
  where
  ns = let zs = [1..9]++map (*10) zs in takeWhile (<nmax) zs
  cs = [ (n,inc) | inc<-[1,10,100],n<-ns,(n-1)*inc<nmax]
  benchPure f !n !inc = bench (showTestCase n inc) $
          nf (\ (a,b,c,d,e) -> f a b c d e) (n,V.unsafeTake (1+(n-1)*inc) u,inc,V.unsafeTake (1+(n-1)*inc) v,inc)
  benchIO f !n !inc = bench (showTestCase n inc) $
       nfIO $ f n (V.unsafeTake (1+(n-1)*inc) u) inc (V.unsafeTake (1+(n-1)*inc) v) inc
  showTestCase n inc = "scopy("++show n++",u,"++show inc++",v,"++show inc++")"

-- | benchmarks for the srot function
srot_benchs :: Int -> V.Vector Float -> V.Vector Float -> Float -> Float -> Benchmark
srot_benchs nmax u v c s = bgroup "srot"
  [ bgroup "stream"   [ benchPure srot n inc | (n,inc)<-cs]
  , bgroup "unsafe"   [ benchIO FORTRAN.srot_unsafe n inc | (n,inc)<-cs]
  , bgroup "safe"     [ benchIO FORTRAN.srot n inc | (n,inc)<-cs]
  ]
  where
  ns = let zs = [1..9]++map (*10) zs in takeWhile (<nmax) zs
  cs = [ (n,inc) | inc<-[1,10,100],n<-ns,(n-1)*inc<nmax]
  benchPure f !n !inc = bench (showTestCase n inc) $
          nf (\ (a,b,c',d,e,g,h) -> f a b c' d e g h) (n,V.unsafeTake (1+(n-1)*inc) u,inc,V.unsafeTake (1+(n-1)*inc) v,inc,c,s)
  benchIO f !n !inc = bench (showTestCase n inc) $
       nfIO $ f n (V.unsafeTake (1+(n-1)*inc) u) inc (V.unsafeTake (1+(n-1)*inc) v) inc c s
  showTestCase n inc = "srot("++show n++",u,"++show inc++",v,"++show inc++",c,s)"


-- | benchmarks for the sswap function
sswap_benchs :: Int -> V.Vector Float -> V.Vector Float -> Benchmark
sswap_benchs nmax u v = bgroup "sswap"
  [ bgroup "stream"   [ benchPure sswap n inc | (n,inc)<-cs]
  , bgroup "unsafe"   [ benchIO FORTRAN.sswap_unsafe n inc | (n,inc)<-cs]
  , bgroup "safe"     [ benchIO FORTRAN.sswap n inc | (n,inc)<-cs]
  ]
  where
   ns = let zs = [1..9]++map (*10) zs in takeWhile (<nmax) zs
   cs = [ (n,inc) | inc<-[1,10,100],n<-ns,(n-1)*inc<nmax]
   benchPure f !n !inc = bench (showTestCase n inc) $
          nf (\ (a,b,c,d,e) -> f a b c d e) (n,V.unsafeTake (1+(n-1)*inc) u,inc,V.unsafeTake (1+(n-1)*inc) v,inc)
   benchIO f !n !inc = bench (showTestCase n inc) $
       nfIO $ f n (V.unsafeTake (1+(n-1)*inc) u) inc (V.unsafeTake (1+(n-1)*inc) v) inc
   showTestCase n inc = "sswap("++show n++",u,"++show inc++",v,"++show inc++")"

-- | benchmarks for the snrm2 function
snrm2_benchs :: Int -> V.Vector Float -> Ptr Float -> Benchmark
snrm2_benchs nmax u us = bgroup "snrm2"
  [ bgroup "stream"   [ benchPure snrm2 n u inc | inc<-[1,-1], n<-ns]
  , bgroup "unsafe"   [ benchIO FORTRAN.snrm2_unsafe n inc | inc<-[1,-1], n<-ns]
  , bgroup "safe"     [ benchIO FORTRAN.snrm2 n inc | inc<-[1,-1], n<-ns]
  ]
  where
   ns = let zs = [1..9]++map (*10) zs in takeWhile (<nmax) zs
   benchPure f !n x !inc = bench (showTestCase n inc) $
       nf (\ (a,b,c) -> f a b c ) (n,x,inc)
   benchIO f !n !inc = bench (showTestCase n inc) $
       nfIO $ f n us inc
   showTestCase n inc = "snrm2("++show n++",u,"++show inc++")"

-- | benchmarks for the isamax function
isamax_benchs :: Int -> V.Vector Float -> Ptr Float -> Benchmark
isamax_benchs nmax u us = bgroup "isamax"
  [ bgroup "stream"   [ benchPure isamax n u inc | inc<-[1,-1], n<-ns]
  , bgroup "unsafe"   [ benchIO FORTRAN.isamax_unsafe n inc | inc<-[1,-1], n<-ns]
  , bgroup "safe"     [ benchIO FORTRAN.isamax n inc | inc<-[1,-1], n<-ns]
  ]
  where
   ns = let zs = [1..9]++map (*10) zs in takeWhile (<nmax) zs
   benchPure f !n x !inc = bench (showTestCase n inc) $
       nf (\ (a,b,c) -> f a b c ) (n,x,inc)
   benchIO f !n !inc = bench (showTestCase n inc) $
       nfIO $ f n us inc
   showTestCase n inc = "isamax("++show n++",u,"++show inc++")"

-- Benchmarks for the sdsdot function
sdsdot_benchs :: Int -> Float -> V.Vector Float -> V.Vector Float
    -> Ptr Float -> Ptr Float -> Benchmark
sdsdot_benchs nmax a u v us vs = bgroup "sdsdot"
   [ bgroup "stream"   [ benchPure sdsdot n a u v inc | inc<-[1,-1], n<-ns]
   , bgroup "unsafe"   [ benchIO FORTRAN.sdsdot_unsafe n inc | inc<-[1,-1], n<-ns]
   , bgroup "safe"     [ benchIO FORTRAN.sdsdot n inc | inc<-[1,-1], n<-ns]
   ]
   where
   ns = let zs = [1..9]++map (*10) zs in takeWhile (<nmax) zs
   benchPure f !n !sb x y !inc = bench (showTestCase n inc) $
       nf (\ (aa,b,c,d,e,g) -> f aa b c d e g) (n,sb,x,inc,y,inc)
   benchIO f !n !inc = bench (showTestCase n inc) $
       nfIO $ f n a us inc vs inc
   showTestCase n inc = "sdsdot("++show n++",a,u,"++show inc++",v,"++show inc++")"

-- | benchmarks for the isamax function
srotg_benchs :: Float -> Float -> Benchmark
srotg_benchs !sa !sb = bgroup "srotg"
  [ bgroup "haskell"  [ benchPure srotg sa sb]
  , bgroup "unsafe"   [ benchIO FORTRAN.srotg_unsafe ]
  , bgroup "safe"     [ benchIO FORTRAN.srotg ]
  ]
  where
   benchPure f a b = bench showTestCase $
       nf (uncurry f) (a,b)
   benchIO f = bench (showTestCase) $
       nfIO $ f sa sb
   showTestCase  = "srotg(sa,sb)"

-- | benchmarks for the srotmg function
srotmg_benchs :: Float -> Float -> Float -> Float -> Benchmark
srotmg_benchs !sd1 !sd2 !sx1 !sy1 = bgroup "srotmg"
  [ bgroup "haskell"  [ benchPure srotmg sd1 sd2 sx1 sy1]
  , bgroup "unsafe"   [ benchIO FORTRAN.srotmg_unsafe ]
  , bgroup "safe"     [ benchIO FORTRAN.srotmg ]
  ]
  where
   benchPure f a b c d = bench showTestCase $
       nf (\ (aa,bb,cc,dd) -> f aa bb cc dd) (a,b,c,d)
   benchIO f = bench (showTestCase) $
       nfIO $ f sd1 sd2 sx1 sy1
   showTestCase  = "srotmg(sd1,sd2,sx1,sx2)"

-- | benchmarks for the srot function
saxpy_benchs :: Int -> Float -> V.Vector Float -> V.Vector Float -> Benchmark
saxpy_benchs nmax aa u v = bgroup "saxpy"
  [ bgroup "stream"   [ benchPure saxpy n inc | (n,inc)<-cs]
  , bgroup "unsafe"   [ benchIO FORTRAN.saxpy_unsafe n inc | (n,inc)<-cs]
  , bgroup "safe"     [ benchIO FORTRAN.saxpy n inc | (n,inc)<-cs]
  ]
  where
  ns = let zs = [1..9]++map (*10) zs in takeWhile (<nmax) zs
  cs = [ (n,inc) | inc<-[1,10,100],n<-ns,(n-1)*inc<nmax]
  benchPure f !n !inc = bench (showTestCase n inc) $
          nf (\ (a,b,c,d,e,g) -> f a b c d e g) (n,aa,V.unsafeTake (1+(n-1)*inc) u,inc,V.unsafeTake (1+(n-1)*inc) v,inc)
  benchIO f !n !inc = bench (showTestCase n inc) $
       nfIO $ f n aa (V.unsafeTake (1+(n-1)*inc) u) inc (V.unsafeTake (1+(n-1)*inc) v) inc
  showTestCase n inc = "saxpy("++show n++",a,u,"++show inc++",v,"++show inc++")"
