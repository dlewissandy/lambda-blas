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
import qualified OSX

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
   [ bench "naive" $ nf naive (u,v)
   , bench "native" $ nf native (n,u,v)
   , bench "cblas" $ nfIO $ cblas (n,us,vs)
   ]
   ]
   where
   n = V.length u
   native (a,b,c) = sdot a b 1 c 1
   cblas  (a,b,c) = OSX.sdot a b 1 c 1
   naive (a,b) = sdot_zip a b
