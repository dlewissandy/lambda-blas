-- | This module defines the Foreign function calls to CBLAS library.  For
-- full documentation consult <http://www.netlib.org/blas/>.
{-# LANGUAGE ForeignFunctionInterface #-}
module Foreign(sdot,sasum) where

import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable

foreign import ccall "sdot_"
   sdot_foreign :: Ptr Int -> Ptr Float -> Ptr Int -> Ptr Float -> Ptr Int -> IO Float
foreign import ccall "sasum_"
      sasum_foreign :: Ptr Int -> Ptr Float -> Ptr Int -> IO Float

-- | Call the fortran implementation of the sdot function.   For details
-- please see <https://software.intel.com/en-us/node/468398#D4E53C70-D8FA-4095-A800-4203CAFE64FE BLAS documentation>
sdot :: Int       -- The number of summands
    -> Ptr Float  -- A pointer to the vector x
    -> Int        -- The increment to use when traversing x
    -> Ptr Float  -- A pointer to the vector y
    -> Int        -- The increment to use when traversing y
    -> IO Float   -- The sum of the elementwise products of x and y.
sdot n px incx py incy =
    alloca $ \ pn ->
    alloca $ \ pincx ->
    alloca $ \ pincy -> do
        poke pn n
        poke pincx incx
        poke pincy incy
        sdot_foreign pn px pincx py pincy

-- | Call the FORTRAN implementation of the sasum function.   For details
-- please see <https://software.intel.com/en-us/node/468392 BLAS documentation>
sasum :: Int     -- the number of summands
    -> Ptr Float -- a pointer to the vector x
    -> Int       -- the increment to use when traversing x
    -> IO Float  -- The sum of the absolute values of elements of x
sasum n px incx =
    alloca $ \ pn ->
    alloca $ \ pincx -> do
        poke pn n
        poke pincx incx
        sasum_foreign pn px pincx
