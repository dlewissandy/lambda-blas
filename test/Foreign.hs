-- | This module defines the Foreign function calls to CBLAS library.  For
-- full documentation consult <http://www.netlib.org/blas/>.
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards #-}
module Foreign(
    isamax,isamax_unsafe,
    sasum,sasum_unsafe,
    saxpy,saxpy_unsafe,
    scopy,scopy_unsafe,
    sdsdot,sdsdot_unsafe,
    sdot,sdot_unsafe,
    snrm2,snrm2_unsafe,
    srot,srot_unsafe,
    srotg,srotg_unsafe,
    srotm,srotm_unsafe,
    srotmg,srotmg_unsafe,
    sscal,sscal_unsafe,
    sswap,sswap_unsafe,
    ) where

import Numerical.BLAS.Types

import qualified Data.Vector.Storable as V
import  Data.Vector.Storable.Internal
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable


foreign import ccall "isamax_" isamax_foreign :: Ptr Int -> Ptr Float -> Ptr Int -> IO Int
foreign import ccall unsafe "isamax_" isamax_unsafe_ :: Ptr Int -> Ptr Float -> Ptr Int -> IO Int
foreign import ccall "sasum_"  sasum_foreign  :: Ptr Int -> Ptr Float -> Ptr Int -> IO Float
foreign import ccall unsafe "sasum_" sasum_unsafe_  :: Ptr Int -> Ptr Float -> Ptr Int -> IO Float
foreign import ccall        "saxpy_"  saxpy_foreign  :: Ptr Int -> Ptr Float ->  Ptr Float -> Ptr Int -> Ptr Float -> Ptr Int -> IO ()
foreign import ccall unsafe "saxpy_"  saxpy_unsafe_  :: Ptr Int -> Ptr Float ->  Ptr Float -> Ptr Int -> Ptr Float -> Ptr Int -> IO ()
foreign import ccall        "scopy_"  scopy_foreign  :: Ptr Int -> Ptr Float -> Ptr Int -> Ptr Float -> Ptr Int -> IO ()
foreign import ccall unsafe "scopy_"  scopy_unsafe_  :: Ptr Int -> Ptr Float -> Ptr Int -> Ptr Float -> Ptr Int -> IO ()
foreign import ccall "sdot_"   sdot_foreign   :: Ptr Int -> Ptr Float -> Ptr Int -> Ptr Float -> Ptr Int -> IO Float
foreign import ccall unsafe "sdot_"   sdot_unsafe_ :: Ptr Int -> Ptr Float -> Ptr Int -> Ptr Float -> Ptr Int -> IO Float
foreign import ccall        "sdsdot_" sdsdot_foreign :: Ptr Int -> Ptr Float -> Ptr Float -> Ptr Int -> Ptr Float -> Ptr Int -> IO Float
foreign import ccall unsafe "sdsdot_" sdsdot_unsafe_ :: Ptr Int -> Ptr Float -> Ptr Float -> Ptr Int -> Ptr Float -> Ptr Int -> IO Float
foreign import ccall "snrm2_"   snrm2_foreign  :: Ptr Int -> Ptr Float -> Ptr Int -> IO Float
foreign import ccall unsafe "snrm2_"  snrm2_unsafe_  :: Ptr Int -> Ptr Float -> Ptr Int -> IO Float
foreign import ccall        "srot_"   srot_foreign   :: Ptr Int -> Ptr Float -> Ptr Int -> Ptr Float -> Ptr Int -> Ptr Float -> Ptr Float -> IO ()
foreign import ccall unsafe "srot_"   srot_unsafe_   :: Ptr Int -> Ptr Float -> Ptr Int -> Ptr Float -> Ptr Int -> Ptr Float -> Ptr Float -> IO ()
foreign import ccall "srotg_"  srotg_foreign  :: Ptr Float -> Ptr Float -> Ptr Float -> Ptr Float -> IO ()
foreign import ccall unsafe "srotg_" srotg_unsafe_ :: Ptr Float -> Ptr Float -> Ptr Float -> Ptr Float -> IO ()
foreign import ccall        "srotm_"  srotm_foreign  :: Ptr Int -> Ptr Float -> Ptr Int -> Ptr Float -> Ptr Int -> Ptr Float -> IO ()
foreign import ccall unsafe "srotm_"  srotm_unsafe_  :: Ptr Int -> Ptr Float -> Ptr Int -> Ptr Float -> Ptr Int -> Ptr Float -> IO ()
foreign import ccall        "srotmg_" srotmg_foreign :: Ptr Float -> Ptr Float -> Ptr Float -> Ptr Float -> Ptr Float -> IO ()
foreign import ccall unsafe "srotmg_" srotmg_unsafe_ :: Ptr Float -> Ptr Float -> Ptr Float -> Ptr Float -> Ptr Float -> IO ()
foreign import ccall        "sscal_"  sscal_foreign  :: Ptr Int -> Ptr Float -> Ptr Float -> Ptr Int -> IO ()
foreign import ccall unsafe "sscal_"  sscal_unsafe_  :: Ptr Int -> Ptr Float -> Ptr Float -> Ptr Int -> IO ()
foreign import ccall        "sswap_"  sswap_foreign  :: Ptr Int -> Ptr Float -> Ptr Int -> Ptr Float -> Ptr Int -> IO ()
foreign import ccall unsafe "sswap_"  sswap_unsafe_  :: Ptr Int -> Ptr Float -> Ptr Int -> Ptr Float -> Ptr Int -> IO ()

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

-- Setup a Givens rotation.
-- please see <http://www.netlib.org/lapack/explore-html/df/d28/group__single__blas__level1_ga2f65d66137ddaeb7ae93fcc4902de3fc.html#ga2f65d66137ddaeb7ae93fcc4902de3fc>
srotg :: Float -> Float -> IO (GivensRot Float)
srotg sa sb         = ffgivens_foreign srotg_foreign sa sb
srotg_unsafe :: Float -> Float -> IO (GivensRot Float)
srotg_unsafe sa sb  = ffgivens_foreign srotg_unsafe_ sa sb

-- Setup a modified Givens rotation.
-- please see <http://www.netlib.org/lapack/explore-html/df/d28/group__single__blas__level1_ga2f65d66137ddaeb7ae93fcc4902de3fc.html#ga2f65d66137ddaeb7ae93fcc4902de3fc>
srot :: Int -> V.Vector Float -> Int -> V.Vector Float -> Int -> Float -> Float -> IO (V.Vector Float, V.Vector Float)
srot  = srotHelper srot_foreign
srot_unsafe :: Int -> V.Vector Float -> Int -> V.Vector Float -> Int -> Float -> Float -> IO (V.Vector Float, V.Vector Float)
srot_unsafe = srotHelper srot_unsafe_
srotHelper :: (Ptr Int -> Ptr Float -> Ptr Int -> Ptr Float -> Ptr Int -> Ptr Float -> Ptr Float -> IO ())
   -> Int -> V.Vector Float -> Int -> V.Vector Float -> Int -> Float -> Float -> IO (V.Vector Float, V.Vector Float)
srotHelper f n u incx v incy c s =
    alloca $ \ pn ->
    alloca $ \ pincx ->
    alloca $ \ pincy ->
    alloca $ \ pc ->
    alloca $ \ ps -> do
        poke pn n
        poke pincx incx
        poke pincy incy
        poke pc c
        poke ps s
        V.MVector sx fptrx <- V.thaw u
        V.MVector sy fptry <- V.thaw v
        f pn (getPtr fptrx) pincx (getPtr fptry) pincy pc ps
        xs <- V.unsafeFreeze $ V.MVector sx fptrx
        ys <- V.unsafeFreeze $ V.MVector sy fptry
        return (xs, ys)

-- Foreign call to saxpy function.
-- please see <http://www.netlib.org/lapack/explore-html/df/d28/group__single__blas__level1_ga2f65d66137ddaeb7ae93fcc4902de3fc.html#ga2f65d66137ddaeb7ae93fcc4902de3fc>
saxpy :: Int -> Float -> V.Vector Float -> Int -> V.Vector Float -> Int -> IO (V.Vector Float)
saxpy  = saxpyHelper saxpy_foreign
saxpy_unsafe :: Int -> Float -> V.Vector Float -> Int -> V.Vector Float -> Int -> IO (V.Vector Float)
saxpy_unsafe = saxpyHelper saxpy_unsafe_
saxpyHelper :: (Ptr Int -> Ptr Float -> Ptr Float -> Ptr Int -> Ptr Float -> Ptr Int -> IO ())
   -> Int -> Float -> V.Vector Float -> Int -> V.Vector Float -> Int -> IO (V.Vector Float)
saxpyHelper f n a u incx v incy =
    alloca $ \ pn ->
    alloca $ \ pincx ->
    alloca $ \ pincy ->
    alloca $ \ pa -> do
        poke pn n
        poke pincx incx
        poke pincy incy
        poke pa a
        V.MVector _ fptrx <- V.unsafeThaw u
        V.MVector sy  fptry <- V.thaw v
        f pn pa (getPtr fptrx) pincx (getPtr fptry) pincy
        V.unsafeFreeze $ V.MVector sy fptry

-- Setup a modified Givens rotation.
-- please see <http://www.netlib.org/lapack/explore-html/df/d28/group__single__blas__level1_ga2f65d66137ddaeb7ae93fcc4902de3fc.html#ga2f65d66137ddaeb7ae93fcc4902de3fc>
srotmg :: Float -> Float -> Float -> Float -> IO (ModGivensRot Float)
srotmg sd1 sd2 sx1 sy1 = modGivensHelper srotmg_foreign sd1 sd2 sx1 sy1
srotmg_unsafe :: Float -> Float -> Float -> Float -> IO (ModGivensRot Float)
srotmg_unsafe sd1 sd2 sx1 sy1 = modGivensHelper srotmg_unsafe_ sd1 sd2 sx1 sy1
modGivensHelper :: (Ptr Float -> Ptr Float -> Ptr Float -> Ptr Float -> Ptr Float -> IO ())
   -> Float -> Float -> Float -> Float -> IO (ModGivensRot Float)
modGivensHelper f sd1 sd2 sx1 sy1 =
    alloca $ \ psd1 ->
    alloca $ \ psd2 ->
    alloca $ \ psx1 ->
    alloca $ \ psy1 ->
    allocaArray 5 $ \ pparms -> do
        poke psd1 sd1
        poke psd2 sd2
        poke psx1 sx1
        poke psy1 sy1
        f psd1 psd2 psx1 psy1 pparms
        d1<-peek psd1
        d2<-peek psd2
        x1<-peek psx1
        [flag,h11,h21,h12,h22] <- peekArray 5 pparms
        return $ case flag of
            -2.0 -> FLAGNEG2
            -1.0 -> FLAGNEG1 {..}
            0.0  -> FLAG0 {..}
            1.0  -> FLAG1 {..}
            _    -> error "unexpected parameter value returned from srotmg"

-- Apply a modified Givens rotation.
-- please see <http://www.netlib.org/lapack/explore-html/df/d28/group__single__blas__level1_ga2f65d66137ddaeb7ae93fcc4902de3fc.html#ga2f65d66137ddaeb7ae93fcc4902de3fc>
srotm :: ModGivensRot Float -> Int -> V.Vector Float -> Int -> V.Vector Float -> Int -> IO (V.Vector Float, V.Vector Float)
srotm = srotmHelper srotm_foreign
srotm_unsafe :: ModGivensRot Float -> Int -> V.Vector Float -> Int -> V.Vector Float -> Int -> IO (V.Vector Float, V.Vector Float)
srotm_unsafe = srotmHelper srotm_unsafe_
srotmHelper :: (Ptr Int -> Ptr Float -> Ptr Int -> Ptr Float -> Ptr Int -> Ptr Float -> IO ())
    -> ModGivensRot Float -> Int -> V.Vector Float -> Int -> V.Vector Float -> Int -> IO (V.Vector Float, V.Vector Float)
{-# INLINE srotmHelper #-}
srotmHelper f flags n sx incx sy incy =
    alloca $ \ pn ->
    alloca $ \ pincx ->
    alloca $ \ pincy ->
    allocaArray 5 $ \ pparms -> do
        poke pn n
        poke pincx incx
        poke pincy incy
        case flags of
            FLAGNEG2 -> pokeArray pparms [-2.0,1.0,0.0,0.0,1.0]
            FLAGNEG1{..} -> pokeArray pparms [-1.0,h11,h21,h12,h22]
            FLAG0{..} -> pokeArray pparms [0.0,0.0,h21,h12,0.0]
            FLAG1{..} -> pokeArray pparms [1.0,h11,0.0,0.0,h22]
        V.MVector zx fptrx <- V.thaw sx
        V.MVector zy fptry <- V.thaw sy
        f pn (getPtr fptrx) pincx (getPtr fptry) pincy pparms
        sx' <- V.freeze $ V.MVector zx fptrx
        sy' <- V.freeze $ V.MVector zy fptry
        return (sx',sy')

-- | Call the FORTRAN implementation of the isamax function.   For details
-- please see <https://software.intel.com/en-us/node/468392 BLAS documentation>
sscal :: Int -> Float -> V.Vector Float -> Int -> IO (V.Vector Float)
sscal = scal_helper sscal_foreign
sscal_unsafe :: Int -> Float -> V.Vector Float -> Int -> IO (V.Vector Float)
sscal_unsafe = scal_helper sscal_unsafe_
scal_helper :: Storable a => (Ptr Int -> Ptr a -> Ptr a -> Ptr Int -> IO ())
    -> Int -> a -> V.Vector a -> Int -> IO (V.Vector a)
scal_helper f n sa sx incx =
    alloca $ \ pa ->
    alloca $ \ pn ->
    alloca $ \ pincx -> do
        V.MVector z fptr <- V.thaw sx
        poke pa sa
        poke pn n
        poke pincx incx
        f pn pa (getPtr fptr) pincx
        V.unsafeFreeze $ V.MVector z fptr

-- | Call the FORTRAN implementation of the scopy function.   For details
-- please see <https://software.intel.com/en-us/node/468392 BLAS documentation>
scopy :: Int -> V.Vector Float -> Int -> V.Vector Float -> Int -> IO (V.Vector Float)
scopy = copy_helper scopy_foreign
scopy_unsafe :: Int -> V.Vector Float -> Int -> V.Vector Float -> Int -> IO (V.Vector Float)
scopy_unsafe = copy_helper scopy_unsafe_
copy_helper :: Storable a => (Ptr Int ->  Ptr a -> Ptr Int -> Ptr a -> Ptr Int -> IO ())
    -> Int -> V.Vector a -> Int -> V.Vector a -> Int -> IO (V.Vector a)
{-# INLINE copy_helper #-}
copy_helper f n sx incx sy incy =
    alloca $ \ pn ->
    alloca $ \ pincx ->
    alloca $ \ pincy -> do
        V.MVector _ fptrx <- V.unsafeThaw sx
        V.MVector z fptry <- V.thaw sy
        poke pn n
        poke pincx incx
        poke pincy incy
        f pn (getPtr fptrx) pincx (getPtr fptry) pincy
        V.unsafeFreeze $ V.MVector z fptry

-- | Call the FORTRAN implementation of the sswap function.   For details
-- please see <https://software.intel.com/en-us/node/468392 BLAS documentation>
sswap :: Int -> V.Vector Float -> Int -> V.Vector Float -> Int -> IO (V.Vector Float, V.Vector Float)
sswap = swap_helper sswap_foreign
sswap_unsafe :: Int -> V.Vector Float -> Int -> V.Vector Float -> Int -> IO (V.Vector Float, V.Vector Float)
sswap_unsafe = swap_helper sswap_unsafe_
{-# INLINE swap_helper #-}
swap_helper :: Storable a => (Ptr Int -> Ptr a -> Ptr Int ->  Ptr a -> Ptr Int -> IO ())
    -> Int -> V.Vector a -> Int -> V.Vector a -> Int -> IO (V.Vector a, V.Vector a)
swap_helper f n sx incx sy incy =
    alloca $ \ pn ->
    alloca $ \ pincx ->
    alloca $ \ pincy -> do
        V.MVector zx fptrx <- V.thaw sx
        V.MVector zy fptry <- V.thaw sy
        poke pn n
        poke pincx incx
        poke pincy incy
        f pn (getPtr fptrx) pincx (getPtr fptry) pincy
        u<-V.unsafeFreeze $ V.MVector zx fptrx
        v<-V.unsafeFreeze $ V.MVector zy fptry
        return (u,v)

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

-- a helper function for marshaling functions of the type of the first argument
ffgivens_foreign :: (Ptr Float -> Ptr Float -> Ptr Float -> Ptr Float -> IO ())
    -> Float
    -> Float
    -> IO (GivensRot Float)
ffgivens_foreign f sa sb =
    alloca $ \ psa ->
    alloca $ \ psb ->
    alloca $ \ pc ->
    alloca $ \ ps -> do
        poke psa sa
        poke psb sb
        f psa psb pc ps
        a <- peek psa
        b <- peek psb
        c <- peek pc
        s <- peek ps
        return $ GIVENSROT (a,b,c,s)
