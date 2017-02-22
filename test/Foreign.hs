-- | This module defines the Foreign function calls to CBLAS library.  For
-- full documentation consult <http://www.netlib.org/blas/>.
{-# LANGUAGE ForeignFunctionInterface #-}
module Foreign(
    isamax,isamax_unsafe,
    sasum,sasum_unsafe,
--    saxpy,
--    scopy,
    sdsdot,sdsdot_unsafe,
    sdot,sdot_unsafe,
    snrm2,snrm2_unsafe,
--    srot,
--    srotg,
--    srotm,
--    srotmg,
--    sscal,
--    sswap,
    ) where

import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable

foreign import ccall "isamax_" isamax_foreign :: Ptr Int -> Ptr Float -> Ptr Int -> IO Int
foreign import ccall unsafe "isamax_" isamax_unsafe_ :: Ptr Int -> Ptr Float -> Ptr Int -> IO Int
foreign import ccall "sasum_"  sasum_foreign  :: Ptr Int -> Ptr Float -> Ptr Int -> IO Float
foreign import ccall unsafe "sasum_" sasum_unsafe_  :: Ptr Int -> Ptr Float -> Ptr Int -> IO Float
--foreign import ccall "saxpy_"  saxpy_foreign  :: Ptr Int -> Ptr Float ->  Ptr Float -> Ptr Int -> Ptr Float -> Ptr Int -> IO ()
--foreign import ccall "scnrm2_" scnrm2_foreign :: Ptr Int -> Ptr Float -> Ptr Int -> IO Float
--foreign import ccall "scopy_"  scopy_foreign  :: Ptr Int -> Ptr Float -> Ptr Int -> Ptr Float -> Ptr Int -> IO ()
foreign import ccall "sdot_"   sdot_foreign   :: Ptr Int -> Ptr Float -> Ptr Int -> Ptr Float -> Ptr Int -> IO Float
foreign import ccall unsafe "sdot_"   sdot_unsafe_ :: Ptr Int -> Ptr Float -> Ptr Int -> Ptr Float -> Ptr Int -> IO Float
foreign import ccall "sdsdot_" sdsdot_foreign :: Ptr Int -> Ptr Float -> Ptr Float -> Ptr Int -> Ptr Float -> Ptr Int -> IO Float
foreign import ccall unsafe "sdsdot_" sdsdot_unsafe_ :: Ptr Int -> Ptr Float -> Ptr Float -> Ptr Int -> Ptr Float -> Ptr Int -> IO Float
foreign import ccall "snrm2_"   snrm2_foreign  :: Ptr Int -> Ptr Float -> Ptr Int -> IO Float
foreign import ccall unsafe "snrm2_"  snrm2_unsafe_  :: Ptr Int -> Ptr Float -> Ptr Int -> IO Float
--foreign import ccall "srot_"   srot_foreign   :: Ptr Int -> Ptr Float -> Ptr Int -> Ptr Float -> Ptr Int -> Ptr Float -> Ptr Float -> IO ()
--foreign import ccall "srotg_"  srotg_foreign  :: Ptr Float -> Ptr Float -> Ptr Float -> Ptr Float -> IO ()
--foreign import ccall "srotm_"  srotm_foreign  :: Ptr Int -> Ptr Float -> Ptr Int -> Ptr Float -> Ptr Int -> Ptr Float -> IO ()
--foreign import ccall "srotmg_" srotmg_foreign :: Ptr Float -> Ptr Float -> Ptr Float -> Ptr Float -> Ptr Float -> IO ()
--foreign import ccall "sscal_"  sscal_foreign  :: Ptr Int -> Ptr Float -> Ptr Float -> Ptr Int -> IO ()
--foreign import ccall "sswap_"  sswap_foreign  :: Ptr Int -> Ptr Float -> Ptr Int -> Ptr Float -> Ptr Int -> IO ()


-- | Call the fortran implementation of the sdot function.   For details
-- please see <https://software.intel.com/en-us/node/468398#D4E53C70-D8FA-4095-A800-4203CAFE64FE BLAS documentation>
sdot :: Int       -- The number of summands
    -> Ptr Float  -- A pointer to the vector x
    -> Int        -- The increment to use when traversing x
    -> Ptr Float  -- A pointer to the vector y
    -> Int        -- The increment to use when traversing y
    -> IO Float   -- The sum of the elementwise products of x and y.
sdot = iviivi_foreign sdot_foreign

sdot_unsafe :: Int -> Ptr Float -> Int -> Ptr Float -> Int -> IO Float
sdot_unsafe = iviivi_foreign sdot_unsafe_

-- | Call the FORTRAN implementation of the sasum function.   For details
-- please see <https://software.intel.com/en-us/node/468392 BLAS documentation>
sasum :: Int -> Ptr Float -> Int -> IO Float
sasum = ivi_foreign sasum_foreign
sasum_unsafe :: Int -> Ptr Float -> Int -> IO Float
sasum_unsafe = ivi_foreign sasum_unsafe_


-- | Call the FORTRAN implementation of the snrm2 function.   For details
-- please see <https://software.intel.com/en-us/node/468392 BLAS documentation>
snrm2 :: Int -> Ptr Float -> Int -> IO Float
snrm2 = ivi_foreign snrm2_foreign
snrm2_unsafe :: Int -> Ptr Float -> Int -> IO Float
snrm2_unsafe = ivi_foreign snrm2_unsafe_


-- | Call the FORTRAN implementation of the isamax function.   For details
-- please see <https://software.intel.com/en-us/node/468392 BLAS documentation>
isamax :: Int -> Ptr Float -> Int -> IO Int
isamax = ivi_foreign isamax_foreign
isamax_unsafe :: Int -> Ptr Float -> Int -> IO Int
isamax_unsafe = ivi_foreign isamax_unsafe_

-- | Call the FORTRAN implementation of the sdsdot function.   For details
-- please see <https://software.intel.com/en-us/node/468392 BLAS documentation>
sdsdot :: Int -> Float -> Ptr Float -> Int -> Ptr Float -> Int -> IO Float
sdsdot n a px incx py incy =
     alloca $ \ pa -> do
        poke pa a
        iviivi_foreign (\ pn _ pincx _ pincy ->
            sdsdot_foreign pn pa px pincx py pincy ) n px incx py incy
sdsdot_unsafe :: Int -> Float -> Ptr Float -> Int -> Ptr Float -> Int -> IO Float
sdsdot_unsafe n a px incx py incy =
     alloca $ \ pa -> do
        poke pa a
        iviivi_foreign (\ pn _ pincx _ pincy ->
            sdsdot_unsafe_ pn pa px pincx py pincy ) n px incx py incy


-- =============================================================================
-- HELPER FUNCTIONS
-- =============================================================================

-- A helper function for wrapping a foreign call to a function of the type
-- of the first argument.
ivi_foreign :: (Ptr Int -> Ptr Float -> Ptr Int -> IO a)
  -> Int
  -> Ptr Float
  -> Int
  -> IO a
ivi_foreign f n px incx =
    alloca $ \ pn ->
    alloca $ \ pincx -> do
        poke pn n
        poke pincx incx
        f pn px pincx

-- A helper function for wrapping a foreign call to a function of the type
-- of the first argument.
iviivi_foreign :: (Ptr Int -> Ptr Float -> Ptr Int
    -> Ptr Float -> Ptr Int -> IO a)
    -> Int
    -> Ptr Float
    -> Int
    -> Ptr Float
    -> Int
    -> IO a
iviivi_foreign f n px incx py incy =
    alloca $ \ pn ->
    alloca $ \ pincx ->
    alloca $ \ pincy -> do
        poke pn n
        poke pincx incx
        poke pincy incy
        f pn px pincx py pincy
