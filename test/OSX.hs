-- | This module defines the Foreign function calls to CBLAS library.  For
-- full documentation consult <http://www.netlib.org/blas/>.
{-# LANGUAGE ForeignFunctionInterface #-}
module OSX(sdot) where

import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable

foreign import ccall "sdot_"
   sdot_foreign :: Ptr Int -> Ptr Float -> Ptr Int -> Ptr Float -> Ptr Int -> IO Float

sdot :: Int -> Ptr Float -> Int -> Ptr Float -> Int -> IO Float
sdot n px incx py incy =
    alloca $ \ pn ->
    alloca $ \ pincx ->
    alloca $ \ pincy -> do
        poke pn n
        poke pincx incx
        poke pincy incy
        sdot_foreign pn px pincx py pincy
