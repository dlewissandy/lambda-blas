{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
module Numerical.BLAS.Types( GivensRot(..), ModGivensRot(..)) where

import GHC.Generics (Generic)
import Control.DeepSeq

data GivensRot a = GIVENSROT  {-# UNPACK #-} !(a,a,a,a) deriving (Eq,Show,Generic)
instance (NFData a)=>NFData (GivensRot a)

-- | ModGivensRot is a data structure for containing the linear operator
-- obtained from the rotmg function.   The values h11, h12, h21, and h22 are
-- the elements of the linear operator.   The four cases correspond two
-- the value of the SFLAG.
data ModGivensRot a
   = FLAGNEG2
   | FLAGNEG1 { d1,d2,x1,h11, h12, h21, h22 :: !a }
   | FLAG0 { d1,d2,x1,h12, h21 :: !a }
   | FLAG1 { d1,d2,x1,h11, h22 :: !a }
   deriving (Eq,Show,Generic)
instance (NFData a)=>NFData (ModGivensRot a)
