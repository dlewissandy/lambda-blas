-- | This module defines the Foreign function calls to CBLAS library.  For
-- full documentation consult <http://www.netlib.org/blas/>.
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
module Foreign(
    isamax,isamax_unsafe,idamax,idamax_unsafe,
    sasum,sasum_unsafe,dasum,dasum_unsafe,
    saxpy,saxpy_unsafe,daxpy,daxpy_unsafe,
    scopy,scopy_unsafe,dcopy,dcopy_unsafe,
    sdsdot,sdsdot_unsafe,
    sdot,sdot_unsafe,ddot,ddot_unsafe,
    snrm2,snrm2_unsafe,dnrm2,dnrm2_unsafe,
    srot,srot_unsafe,drot,drot_unsafe,
    srotg,srotg_unsafe,drotg,drotg_unsafe,
    srotm,srotm_unsafe,drotm,drotm_unsafe,
    srotmg,srotmg_unsafe,drotmg,drotmg_unsafe,
    sscal,sscal_unsafe,dscal,dscal_unsafe,
    sswap,sswap_unsafe,dswap,dswap_unsafe,
    ) where

import Numerical.BLAS.Types

import qualified Data.Vector.Storable as V
import  Data.Vector.Storable.Internal
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable


foreign import ccall        "isamax_" isamax_foreign :: Ptr Int -> Ptr Float -> Ptr Int -> IO Int
foreign import ccall unsafe "isamax_" isamax_unsafe_ :: Ptr Int -> Ptr Float -> Ptr Int -> IO Int
foreign import ccall        "idamax_" idamax_foreign :: Ptr Int -> Ptr Double -> Ptr Int -> IO Int
foreign import ccall unsafe "idamax_" idamax_unsafe_ :: Ptr Int -> Ptr Double -> Ptr Int -> IO Int
foreign import ccall        "sasum_"  sasum_foreign  :: Ptr Int -> Ptr Float -> Ptr Int -> IO Float
foreign import ccall unsafe "sasum_" sasum_unsafe_  :: Ptr Int -> Ptr Float -> Ptr Int -> IO Float
foreign import ccall        "dasum_" dasum_foreign  :: Ptr Int -> Ptr Double -> Ptr Int -> IO Double
foreign import ccall unsafe "dasum_" dasum_unsafe_  :: Ptr Int -> Ptr Double -> Ptr Int -> IO Double
foreign import ccall        "saxpy_"  saxpy_foreign  :: Ptr Int -> Ptr Float ->  Ptr Float -> Ptr Int -> Ptr Float -> Ptr Int -> IO ()
foreign import ccall unsafe "saxpy_"  saxpy_unsafe_  :: Ptr Int -> Ptr Float ->  Ptr Float -> Ptr Int -> Ptr Float -> Ptr Int -> IO ()
foreign import ccall        "daxpy_"  daxpy_foreign  :: Ptr Int -> Ptr Double ->  Ptr Double -> Ptr Int -> Ptr Double -> Ptr Int -> IO ()
foreign import ccall unsafe "daxpy_"  daxpy_unsafe_  :: Ptr Int -> Ptr Double ->  Ptr Double -> Ptr Int -> Ptr Double -> Ptr Int -> IO ()
foreign import ccall        "scopy_"  scopy_foreign  :: Ptr Int -> Ptr Float -> Ptr Int -> Ptr Float -> Ptr Int -> IO ()
foreign import ccall unsafe "scopy_"  scopy_unsafe_  :: Ptr Int -> Ptr Float -> Ptr Int -> Ptr Float -> Ptr Int -> IO ()
foreign import ccall        "dcopy_"  dcopy_foreign  :: Ptr Int -> Ptr Double -> Ptr Int -> Ptr Double -> Ptr Int -> IO ()
foreign import ccall unsafe "dcopy_"  dcopy_unsafe_  :: Ptr Int -> Ptr Double -> Ptr Int -> Ptr Double -> Ptr Int -> IO ()
foreign import ccall        "sdot_"   sdot_foreign   :: Ptr Int -> Ptr Float -> Ptr Int -> Ptr Float -> Ptr Int -> IO Float
foreign import ccall unsafe "sdot_"   sdot_unsafe_ :: Ptr Int -> Ptr Float -> Ptr Int -> Ptr Float -> Ptr Int -> IO Float
foreign import ccall        "ddot_"   ddot_foreign   :: Ptr Int -> Ptr Double -> Ptr Int -> Ptr Double -> Ptr Int -> IO Double
foreign import ccall unsafe "ddot_"   ddot_unsafe_ :: Ptr Int -> Ptr Double -> Ptr Int -> Ptr Double -> Ptr Int -> IO Double
foreign import ccall        "sdsdot_" sdsdot_foreign :: Ptr Int -> Ptr Float -> Ptr Float -> Ptr Int -> Ptr Float -> Ptr Int -> IO Float
foreign import ccall unsafe "sdsdot_" sdsdot_unsafe_ :: Ptr Int -> Ptr Float -> Ptr Float -> Ptr Int -> Ptr Float -> Ptr Int -> IO Float
foreign import ccall        "snrm2_"  snrm2_foreign  :: Ptr Int -> Ptr Float -> Ptr Int -> IO Float
foreign import ccall unsafe "snrm2_"  snrm2_unsafe_  :: Ptr Int -> Ptr Float -> Ptr Int -> IO Float
foreign import ccall        "dnrm2_"  dnrm2_foreign  :: Ptr Int -> Ptr Double -> Ptr Int -> IO Double
foreign import ccall unsafe "dnrm2_"  dnrm2_unsafe_  :: Ptr Int -> Ptr Double -> Ptr Int -> IO Double
foreign import ccall        "srot_"   srot_foreign   :: Ptr Int -> Ptr Float -> Ptr Int -> Ptr Float -> Ptr Int -> Ptr Float -> Ptr Float -> IO ()
foreign import ccall unsafe "srot_"   srot_unsafe_   :: Ptr Int -> Ptr Float -> Ptr Int -> Ptr Float -> Ptr Int -> Ptr Float -> Ptr Float -> IO ()
foreign import ccall        "drot_"   drot_foreign   :: Ptr Int -> Ptr Double -> Ptr Int -> Ptr Double -> Ptr Int -> Ptr Double -> Ptr Double -> IO ()
foreign import ccall unsafe "drot_"   drot_unsafe_   :: Ptr Int -> Ptr Double -> Ptr Int -> Ptr Double -> Ptr Int -> Ptr Double -> Ptr Double -> IO ()
foreign import ccall        "srotg_"  srotg_foreign  :: Ptr Float -> Ptr Float -> Ptr Float -> Ptr Float -> IO ()
foreign import ccall unsafe "srotg_" srotg_unsafe_ :: Ptr Float -> Ptr Float -> Ptr Float -> Ptr Float -> IO ()
foreign import ccall        "drotg_"  drotg_foreign  :: Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> IO ()
foreign import ccall unsafe "drotg_"  drotg_unsafe_  :: Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> IO ()
foreign import ccall        "srotm_"  srotm_foreign  :: Ptr Int -> Ptr Float -> Ptr Int -> Ptr Float -> Ptr Int -> Ptr Float -> IO ()
foreign import ccall unsafe "srotm_"  srotm_unsafe_  :: Ptr Int -> Ptr Float -> Ptr Int -> Ptr Float -> Ptr Int -> Ptr Float -> IO ()
foreign import ccall        "drotm_"  drotm_foreign  :: Ptr Int -> Ptr Double -> Ptr Int -> Ptr Double -> Ptr Int -> Ptr Double -> IO ()
foreign import ccall unsafe "drotm_"  drotm_unsafe_  :: Ptr Int -> Ptr Double -> Ptr Int -> Ptr Double -> Ptr Int -> Ptr Double -> IO ()
foreign import ccall        "srotmg_" srotmg_foreign :: Ptr Float -> Ptr Float -> Ptr Float -> Ptr Float -> Ptr Float -> IO ()
foreign import ccall unsafe "srotmg_" srotmg_unsafe_ :: Ptr Float -> Ptr Float -> Ptr Float -> Ptr Float -> Ptr Float -> IO ()
foreign import ccall        "drotmg_" drotmg_foreign :: Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> IO ()
foreign import ccall unsafe "drotmg_" drotmg_unsafe_ :: Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> IO ()
foreign import ccall        "sscal_"  sscal_foreign  :: Ptr Int -> Ptr Float -> Ptr Float -> Ptr Int -> IO ()
foreign import ccall unsafe "sscal_"  sscal_unsafe_  :: Ptr Int -> Ptr Float -> Ptr Float -> Ptr Int -> IO ()
foreign import ccall        "dscal_"  dscal_foreign  :: Ptr Int -> Ptr Double -> Ptr Double -> Ptr Int -> IO ()
foreign import ccall unsafe "dscal_"  dscal_unsafe_  :: Ptr Int -> Ptr Double -> Ptr Double -> Ptr Int -> IO ()
foreign import ccall        "sswap_"  sswap_foreign  :: Ptr Int -> Ptr Float -> Ptr Int -> Ptr Float -> Ptr Int -> IO ()
foreign import ccall unsafe "sswap_"  sswap_unsafe_  :: Ptr Int -> Ptr Float -> Ptr Int -> Ptr Float -> Ptr Int -> IO ()
foreign import ccall        "dswap_"  dswap_foreign  :: Ptr Int -> Ptr Double -> Ptr Int -> Ptr Double -> Ptr Int -> IO ()
foreign import ccall unsafe "dswap_"  dswap_unsafe_  :: Ptr Int -> Ptr Double -> Ptr Int -> Ptr Double -> Ptr Int -> IO ()
-- | Call the fortran implementation of the sdot function.   For details
-- please see <https://software.intel.com/en-us/node/468398#D4E53C70-D8FA-4095-A800-4203CAFE64FE BLAS documentation>
sdot,sdot_unsafe :: Int -> V.Vector Float -> Int -> V.Vector Float -> Int -> IO Float
sdot = iviivi_foreign sdot_foreign
sdot_unsafe = iviivi_foreign sdot_unsafe_
ddot,ddot_unsafe :: Int -> V.Vector Double -> Int -> V.Vector Double -> Int -> IO Double
ddot = iviivi_foreign ddot_foreign
ddot_unsafe = iviivi_foreign ddot_unsafe_

-- | Call the FORTRAN implementation of the sasum function.   For details
-- please see <https://software.intel.com/en-us/node/468392 BLAS documentation>
sasum, sasum_unsafe :: Int -> V.Vector Float -> Int -> IO Float
sasum = ivi_foreign sasum_foreign
sasum_unsafe = ivi_foreign sasum_unsafe_
dasum,dasum_unsafe :: Int -> V.Vector Double -> Int -> IO Double
dasum = ivi_foreign dasum_foreign
dasum_unsafe = ivi_foreign dasum_unsafe_

-- | Call the FORTRAN implementation of the snrm2 function.   For details
-- please see <https://software.intel.com/en-us/node/468392 BLAS documentation>
snrm2,snrm2_unsafe :: Int -> V.Vector Float -> Int -> IO Float
snrm2 = ivi_foreign snrm2_foreign
snrm2_unsafe = ivi_foreign snrm2_unsafe_
dnrm2,dnrm2_unsafe :: Int -> V.Vector Double -> Int -> IO Double
dnrm2 = ivi_foreign dnrm2_foreign
dnrm2_unsafe = ivi_foreign dnrm2_unsafe_

-- | Call the FORTRAN implementation of the isamax function.   For details
-- please see <https://software.intel.com/en-us/node/468392 BLAS documentation>
isamax,isamax_unsafe :: Int -> V.Vector Float -> Int -> IO Int
isamax = ivi_foreign isamax_foreign
isamax_unsafe = ivi_foreign isamax_unsafe_
idamax,idamax_unsafe :: Int -> V.Vector Double -> Int -> IO Int
idamax = ivi_foreign idamax_foreign
idamax_unsafe = ivi_foreign idamax_unsafe_

-- | Call the FORTRAN implementation of the sdsdot function.   For details
-- please see <https://software.intel.com/en-us/node/468392 BLAS documentation>
sdsdot :: Int -> Float -> V.Vector Float -> Int -> V.Vector Float -> Int -> IO Float
sdsdot  = sdsdotHelper sdsdot_foreign
sdsdot_unsafe :: Int -> Float -> V.Vector Float -> Int -> V.Vector Float -> Int -> IO Float
sdsdot_unsafe = sdsdotHelper sdsdot_unsafe_
sdsdotHelper :: (Ptr Int -> Ptr Float -> Ptr Float -> Ptr Int -> Ptr Float -> Ptr Int -> IO Float)
    -> Int -> Float -> V.Vector Float -> Int -> V.Vector Float -> Int -> IO Float
{-# INLINE sdsdotHelper #-}
sdsdotHelper f n a sx incx sy incy =
    alloca $ \ pn ->
    alloca $ \ pincx ->
    alloca $ \ pincy ->
    alloca $ \ pa -> do
        poke pn n
        poke pincx incx
        poke pincy incy
        poke pa a
        V.MVector _ fptrx <- V.unsafeThaw sx
        V.MVector _ fptry <- V.unsafeThaw sy
        f pn pa (getPtr fptrx) pincx (getPtr fptry) pincy

-- Setup a Givens rotation.
-- please see <http://www.netlib.org/lapack/explore-html/df/d28/group__single__blas__level1_ga2f65d66137ddaeb7ae93fcc4902de3fc.html#ga2f65d66137ddaeb7ae93fcc4902de3fc>
drotg,drotg_unsafe :: Double -> Double -> IO (GivensRot Double)
drotg = rotgHelper drotg_foreign
drotg_unsafe = rotgHelper drotg_unsafe_
srotg,srotg_unsafe :: Float -> Float -> IO (GivensRot Float)
srotg = rotgHelper srotg_foreign
srotg_unsafe = rotgHelper srotg_unsafe_
-- a helper function for marshaling functions of the type of the first argument
rotgHelper :: (V.Storable a)
    => (Ptr a -> Ptr a -> Ptr a -> Ptr a -> IO ())
    -> a -> a -> IO (GivensRot a)
{-# INLINE rotgHelper #-}
rotgHelper f !sa !sb =
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

-- Setup a modified Givens rotation.
-- please see <http://www.netlib.org/lapack/explore-html/df/d28/group__single__blas__level1_ga2f65d66137ddaeb7ae93fcc4902de3fc.html#ga2f65d66137ddaeb7ae93fcc4902de3fc>
srot,srot_unsafe :: Int -> V.Vector Float -> Int -> V.Vector Float -> Int -> Float -> Float -> IO (V.Vector Float, V.Vector Float)
srot  = rotHelper srot_foreign
srot_unsafe = rotHelper srot_unsafe_
drot,drot_unsafe :: Int -> V.Vector Double -> Int -> V.Vector Double -> Int -> Double -> Double -> IO (V.Vector Double, V.Vector Double)
drot  = rotHelper drot_foreign
drot_unsafe = rotHelper drot_unsafe_
rotHelper :: (V.Storable a) => (Ptr Int -> Ptr a -> Ptr Int -> Ptr a -> Ptr Int -> Ptr a -> Ptr a -> IO ())
   -> Int -> V.Vector a -> Int -> V.Vector a -> Int -> a -> a -> IO (V.Vector a, V.Vector a)
{-# INLINE rotHelper #-}
rotHelper f n u incx v incy c s =
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
saxpy,saxpy_unsafe :: Int -> Float -> V.Vector Float -> Int -> V.Vector Float -> Int -> IO (V.Vector Float)
saxpy  = axpyHelper saxpy_foreign
saxpy_unsafe = axpyHelper saxpy_unsafe_
daxpy,daxpy_unsafe :: Int -> Double -> V.Vector Double -> Int -> V.Vector Double -> Int -> IO (V.Vector Double)
daxpy  = axpyHelper daxpy_foreign
daxpy_unsafe = axpyHelper daxpy_unsafe_
axpyHelper :: (V.Storable a)
   => (Ptr Int -> Ptr a -> Ptr a -> Ptr Int -> Ptr a -> Ptr Int -> IO ())
   -> Int -> a -> V.Vector a -> Int -> V.Vector a -> Int -> IO (V.Vector a)
{-# INLINE axpyHelper #-}
axpyHelper f !n !a sx !incx sy !incy =
    alloca $ \ pn ->
    alloca $ \ pincx ->
    alloca $ \ pincy ->
    alloca $ \ pa -> do
        poke pn n
        poke pincx incx
        poke pincy incy
        poke pa a
        V.MVector _ fptrx   <- V.unsafeThaw sx
        V.MVector !zy fptry <- V.thaw sy
        f pn pa (getPtr fptrx) pincx (getPtr fptry) pincy
        V.unsafeFreeze $ V.MVector zy fptry

-- Setup a modified Givens rotation.
-- please see <http://www.netlib.org/lapack/explore-html/df/d28/group__single__blas__level1_ga2f65d66137ddaeb7ae93fcc4902de3fc.html#ga2f65d66137ddaeb7ae93fcc4902de3fc>
srotmg,srotmg_unsafe :: Float -> Float -> Float -> Float -> IO (ModGivensRot Float)
srotmg sd1 sd2 sx1 sy1 = rotmgHelper srotmg_foreign sd1 sd2 sx1 sy1
srotmg_unsafe sd1 sd2 sx1 sy1 = rotmgHelper srotmg_unsafe_ sd1 sd2 sx1 sy1
drotmg,drotmg_unsafe :: Double -> Double -> Double -> Double -> IO (ModGivensRot Double)
drotmg sd1 sd2 sx1 sy1 = rotmgHelper drotmg_foreign sd1 sd2 sx1 sy1
drotmg_unsafe sd1 sd2 sx1 sy1 = rotmgHelper drotmg_unsafe_ sd1 sd2 sx1 sy1
rotmgHelper :: (V.Storable a, Eq a, Num a) =>
   (Ptr a -> Ptr a -> Ptr a -> Ptr a -> Ptr a -> IO ())
   -> a -> a -> a -> a -> IO (ModGivensRot a)
{-# INLINE rotmgHelper #-}
rotmgHelper f sd1 sd2 sx1 sy1 =
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
            -2 -> FLAGNEG2
            -1 -> FLAGNEG1 {..}
            0  -> FLAG0 {..}
            1  -> FLAG1 {..}
            _    -> error "unexpected parameter value returned from srotmg"

-- Apply a modified Givens rotation.
-- please see <http://www.netlib.org/lapack/explore-html/df/d28/group__single__blas__level1_ga2f65d66137ddaeb7ae93fcc4902de3fc.html#ga2f65d66137ddaeb7ae93fcc4902de3fc>
srotm,srotm_unsafe :: ModGivensRot Float -> Int -> V.Vector Float -> Int -> V.Vector Float -> Int -> IO (V.Vector Float, V.Vector Float)
srotm        = rotmHelper srotm_foreign
srotm_unsafe = rotmHelper srotm_unsafe_
drotm,drotm_unsafe :: ModGivensRot Double -> Int -> V.Vector Double -> Int -> V.Vector Double -> Int -> IO (V.Vector Double, V.Vector Double)
drotm        = rotmHelper drotm_foreign
drotm_unsafe = rotmHelper drotm_unsafe_
rotmHelper :: (V.Storable a, Num a)
    => (Ptr Int -> Ptr a -> Ptr Int -> Ptr a -> Ptr Int -> Ptr a -> IO ())
    -> ModGivensRot a -> Int -> V.Vector a -> Int -> V.Vector a -> Int -> IO (V.Vector a, V.Vector a)
{-# INLINE rotmHelper #-}
rotmHelper f flags n sx incx sy incy =
    alloca $ \ pn ->
    alloca $ \ pincx ->
    alloca $ \ pincy ->
    allocaArray 5 $ \ pparms -> do
        poke pn n
        poke pincx incx
        poke pincy incy
        case flags of
            FLAGNEG2 -> pokeArray pparms [-2,1,0,0,1]
            FLAGNEG1{..} -> pokeArray pparms [-1,h11,h21,h12,h22]
            FLAG0{..} -> pokeArray pparms [0,0,h21,h12,0]
            FLAG1{..} -> pokeArray pparms [1,h11,0,0,h22]
        V.MVector zx fptrx <- V.thaw sx
        V.MVector zy fptry <- V.thaw sy
        f pn (getPtr fptrx) pincx (getPtr fptry) pincy pparms
        sx' <- V.unsafeFreeze $ V.MVector zx fptrx
        sy' <- V.unsafeFreeze $ V.MVector zy fptry
        return (sx',sy')

-- | Call the FORTRAN implementation of the isamax function.   For details
-- please see <https://software.intel.com/en-us/node/468392 BLAS documentation>
sscal,sscal_unsafe :: Int -> Float -> V.Vector Float -> Int -> IO (V.Vector Float)
sscal = scal_helper sscal_foreign
sscal_unsafe = scal_helper sscal_unsafe_
dscal,dscal_unsafe :: Int -> Double -> V.Vector Double -> Int -> IO (V.Vector Double)
dscal = scal_helper dscal_foreign
dscal_unsafe = scal_helper dscal_unsafe_
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
scopy, scopy_unsafe :: Int -> V.Vector Float -> Int -> V.Vector Float -> Int -> IO (V.Vector Float)
scopy = copy_helper scopy_foreign
scopy_unsafe = copy_helper scopy_unsafe_
dcopy, dcopy_unsafe :: Int -> V.Vector Double -> Int -> V.Vector Double -> Int -> IO (V.Vector Double)
dcopy = copy_helper dcopy_foreign
dcopy_unsafe = copy_helper dcopy_unsafe_
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
sswap, sswap_unsafe :: Int -> V.Vector Float -> Int -> V.Vector Float -> Int -> IO (V.Vector Float, V.Vector Float)
sswap        = swap_helper sswap_foreign
sswap_unsafe = swap_helper sswap_unsafe_
dswap, dswap_unsafe :: Int -> V.Vector Double -> Int -> V.Vector Double -> Int -> IO (V.Vector Double, V.Vector Double)
dswap        = swap_helper dswap_foreign
dswap_unsafe = swap_helper dswap_unsafe_
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
ivi_foreign :: (Storable b)
  => (Ptr Int -> Ptr b -> Ptr Int -> IO a)
  -> Int
  -> V.Vector b
  -> Int
  -> IO a
ivi_foreign f n sx incx =
    alloca $ \ pn ->
    alloca $ \ pincx -> do
        poke pn n
        poke pincx incx
        V.MVector _ fptr <- V.unsafeThaw sx
        f pn (getPtr fptr) pincx

-- A helper function for wrapping a foreign call to a function of the type
-- of the first argument.
iviivi_foreign :: (V.Storable b)
    => (Ptr Int -> Ptr b -> Ptr Int -> Ptr b -> Ptr Int -> IO a)
    -> Int
    -> V.Vector b
    -> Int
    -> V.Vector b
    -> Int
    -> IO a
iviivi_foreign f n sx incx sy incy =
    alloca $ \ pn ->
    alloca $ \ pincx ->
    alloca $ \ pincy -> do
        poke pn n
        poke pincx incx
        poke pincy incy
        V.MVector _ fptrx <- V.unsafeThaw sx
        V.MVector _ fptry <- V.unsafeThaw sy
        f pn (getPtr fptrx) pincx (getPtr fptry ) pincy
