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
import qualified OSX as FORTRAN

main :: IO ()
main = do
    u<- generate $ genNVector (genFloat genEveryday) 10001
    v<- generate $ genNVector (genFloat genEveryday) 10001
    withArray (V.toList u) $ \ us ->
       withArray (V.toList v) $ \ vs ->
        defaultMain $ benchmarks u v us vs

benchmarks :: V.Vector Float -> V.Vector Float
           -> Ptr Float -> Ptr Float
           -> [Benchmark]
benchmarks u v us vs = [ bgroup "sdot"
   [ bench "sdot_zip(u,v)" $ nf naive (u,v)
   , bench "sdot(n,u,1,v,1)" $ nf native (n,u,1,v,1)
   , bench "FORTRAN.sdot(n,u,1,v,1)" $ nfIO $ fortran (n,us,1,vs,1)
   , bench "sdot(n,u,-1,v,-1)" $ nf native (n,u,-1,v,-1)
   , bench "FORTRAN.sdot(n,u,-1,v,-1)" $ nfIO $ fortran (n,us,-1,vs,-1)
   ]
   ]
   where
   n = V.length u
   native (a,b,c,d,e) = sdot a b c d e
   fortran  (a,b,c,d,e) = FORTRAN.sdot a b c d e
   naive (a,b) = sdot_zip a b
