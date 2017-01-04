-- | This module defines the Foreign function calls to CBLAS library.  For
-- full documentation consult <http://www.netlib.org/blas/>.
{-# LANGUAGE ForeignFunctionInterface #-}
module OSX(sdot) where

import Foreign.Ptr

foreign import ccall "cblas_sdot"
   sdot :: Int -> Ptr Float -> Int -> Ptr Float -> Int ->  Float
