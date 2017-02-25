{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
module Numerical.BLAS.Types( GivensRot(..)) where

import GHC.Generics (Generic)
import Control.DeepSeq

data GivensRot a = GIVENSROT  {-# UNPACK #-} !(a,a,a,a) deriving (Eq,Show,Generic)

instance (NFData a)=>NFData (GivensRot a)
