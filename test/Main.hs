{-# LANGUAGE ViewPatterns #-}
-- | This test suite performs unit tests to evidence the correctness of
-- the native haskell implementations of BLAS subroutines.
module Main( main ) where

import qualified Data.Vector.Storable as V
import System.Random
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
-- | This package
import Numerical.BLAS.Single
import Numerical.BLAS.Types
-- | This test suite
import Gen
import qualified Foreign as Fortran

-- | Perform the tests
main :: IO ()
main = defaultMain tests

-- | A data structure containing all the tests that can be performed.
tests :: TestTree
tests = testGroup "BLAS"
    [ genTests "Double" (1::Double) -- test the random number generators for IEEE Doubles
    , genTests "Float"  (1::Float)  -- test the random number generators for IEEE Singles
    , testGroup "Level-1"
        [ dotTest "sdot" dot Fortran.sdot genNiceFloat (elements [-5..5])
        , dotTest "ddot" dot Fortran.ddot genNiceDouble (elements [-5..5])
        , sdsdotTest "sdsdot" sdsdot (elements [-5..5])
        , rotgTest "srotg" genNiceFloat Fortran.srotg rotg
        , rotgTest "drotg" genNiceDouble Fortran.drotg rotg
        , rotmgTest "srotmg" rotmg Fortran.srotmg genNiceFloat
        , rotmgTest "drotmg" rotmg Fortran.drotmg genNiceDouble
        , rotmTest "srotm" rotmg rotm Fortran.srotm genNiceFloat (elements [-5..5] `suchThat` (/=0))
        , rotmTest "drotm" rotmg rotm Fortran.drotm genNiceDouble (elements [-5..5] `suchThat` (/=0))
        , iviTest "sasum" asum (Fortran.sasum) genNiceFloat (elements [1..5])
        , iviTest "dasum" asum (Fortran.dasum) genNiceDouble (elements [1..5])
        , iviTest "snrm2" nrm2 (Fortran.snrm2) genNiceFloat (elements [1..5])
        , iviTest "dnrm2" nrm2 (Fortran.dnrm2) genNiceDouble (elements [1..5])
        , iviTest "isamax" (\ n u incx -> succ $ iamax n u incx ) (Fortran.isamax) genNiceFloat (elements [1..5])
        , iviTest "idamax" (\ n u incx -> succ $ iamax n u incx ) (Fortran.idamax) genNiceDouble (elements [1..5])
        , scalTest "sscal" scal Fortran.sscal genNiceFloat  (elements [1..5])
        , scalTest "dscal" scal Fortran.dscal genNiceDouble (elements [1..5])
        , copyTest "scopy" copy Fortran.scopy genNiceFloat  (elements [-5..5])
        , copyTest "dcopy" copy Fortran.dcopy genNiceDouble (elements [-5..5])
        , swapTest "sswap" swap Fortran.sswap genNiceFloat  (elements [-5..5])
        , swapTest "dswap" swap Fortran.dswap genNiceDouble (elements [-5..5])
        , rotTest  "srot"  rot Fortran.srot genNiceFloat (elements [-5,5])
        , rotTest  "drot"  rot Fortran.drot genNiceDouble (elements [-5,5])
        , axpyTest "saxpy" axpy Fortran.saxpy genNiceFloat  (elements [-5,5])
        , axpyTest "daxpy" axpy Fortran.daxpy genNiceDouble (elements [-5,5])
        ]
    ]

-- | Evidence that the native sdot function is byte equivalent to the CBLAS
-- implementation.  Vectors of length 1-10 are tested having elements that are
-- in the range of approximately (epsilon/2,2/epsilon)
dotTest :: (V.Storable a, Eq a, Show a)
    => String
    -> (Int -> V.Vector a -> Int -> V.Vector a -> Int -> a)
    -> (Int -> V.Vector a -> Int -> V.Vector a -> Int -> IO a)
    -> Gen a
    -> Gen Int
    -> TestTree
dotTest testname func ref gen genInc = testProperty testname $
    -- Choose the length of the vector
    forAll (choose (1,100)) $ \ n ->
    -- Randomly generate two vectors of the chosen length
    forAll (genInc) $ \ incx ->
    forAll (genInc) $ \ incy ->
    forAll (genNVector gen (1+(n-1)*abs incx )) $ \ u ->
    forAll (genNVector gen (1+(n-1)*abs incy )) $ \ v ->
       -- monadically marshal the vectors into arrays for use with CBLAS
       ioProperty $ do
           -- compute the expected and observed values
           expected <- ref n u incx v incy
           let observed = func n u incx v incy
           runTest expected observed

-- | Evidence that the native sdsdot function is byte equivalent to the BLAS
-- implementation.  Vectors of length 1-100 are tested having elements that are
-- in the range of approximately +/-(epsilon/2,2/epsilon)
sdsdotTest :: String
        -> (Int -> Float -> V.Vector Float -> Int -> V.Vector Float -> Int -> Float)
        -> Gen Int
        -> TestTree
sdsdotTest testname func genInc = testProperty testname $
   -- Choose the length of the vector
   forAll (choose (1,100)) $ \ n ->
   forAll genNiceFloat $ \ a ->
   -- Randomly generate two vectors of the chosen length
   forAll (genInc) $ \ incx ->
   forAll (genInc) $ \ incy ->
   forAll (genNVector genNiceFloat (1+(n-1)*abs incx )) $ \ u ->
   forAll (genNVector genNiceFloat (1+(n-1)*abs incy )) $ \ v ->
      -- monadically marshal the vectors into arrays for use with CBLAS
      ioProperty $ do
          -- compute the expected and observed values
          expected <- Fortran.sdsdot n a u incx v incy
          let observed = func n a u incx v incy
          runTest expected observed

-- | Evidence that the native sswap function is byte equivalent to the BLAS
-- implementation.  Vectors of length 1-100 are tested having elements that are
-- in the range of approximately +/-(epsilon/2,2/epsilon)
swapTest :: (V.Storable a, Eq a, Show a)
    => String
    -> (Int ->  V.Vector a -> Int -> V.Vector a -> Int -> (V.Vector a, V.Vector a))
    -> (Int ->  V.Vector a -> Int -> V.Vector a -> Int -> IO (V.Vector a, V.Vector a))
    -> Gen a
    -> Gen Int
    -> TestTree
swapTest testname func ref gen genInc = testProperty testname $
   -- Choose the length of the vector.   Vectors will have a length of (1+(n-1)*abs inc)+m)
   forAll (choose (1,5)) $ \ n ->
   forAll (choose (0,2)) $ \ mx ->
   forAll (choose (0,2)) $ \ my ->
   -- Randomly generate two vectors of the chosen length
   forAll (genInc `suchThat` (/=0)) $ \ incx ->
   forAll (genInc `suchThat` (/=0)) $ \ incy ->
   forAll (genNVector gen (mx+1+(n-1)*abs incx )) $ \ u ->
   forAll (genNVector gen (my+1+(n-1)*abs incy )) $ \ v ->
      -- monadically marshal the vectors into arrays for use with CBLAS
      ioProperty $ do
          -- compute the expected and observed values
          expected <- ref n u incx v incy
          let observed = func n u incx v incy
          runTest expected observed

-- | Evidence that the native a rotg function is byte equivalent to the BLAS
-- implementation.  Parameter values that are in the range of approximately
-- +/-(epsilon/2,2/epsilon) are tested.
rotgTest :: (Eq a,Show a)
    => String  -- The test name
    -> Gen a   -- a generator for elements in the underlying field
    -> ( a -> a -> IO (GivensRot a))  -- The FORTRAN reference function
    -> (a -> a -> GivensRot a )       -- The Test function.
    -> TestTree
rotgTest testname gen ref func = testProperty testname $
   forAll gen $ \ sa ->
   forAll gen $ \ sb ->
      -- monadically marshal the vectors into arrays for use with CBLAS
      ioProperty $ do
          -- compute the expected and observed values
          expected <- ref sa sb
          let observed = func sa sb
          runTest expected observed

-- | Evidence that the native srotg function is byte equivalent to the BLAS
-- implementation.  Parameter values that are in the range of approximately
-- +/-(epsilon/2,2/epsilon) are tested.
rotTest :: (V.Storable a, Floating a, Show a, Eq a)
    => String
    -> ( Int -> V.Vector a -> Int -> V.Vector a -> Int -> a -> a -> (V.Vector a, V.Vector a))
    -> ( Int -> V.Vector a -> Int -> V.Vector a -> Int -> a -> a -> IO (V.Vector a, V.Vector a))
    -> Gen a
    -> Gen Int
    -> TestTree
rotTest testname func ref gen genInc = testProperty testname $
    -- Choose the length of the vector
    forAll (choose (1,100)) $ \ n ->
    forAll gen $ \ a ->
    -- Randomly generate two vectors of the chosen length
    forAll (genInc `suchThat` (/=0)) $ \ incx ->
    forAll (genInc `suchThat` (/=0)) $ \ incy ->
    forAll (genNVector gen (1+(n-1)*abs incx )) $ \ u ->
    forAll (genNVector gen (1+(n-1)*abs incy )) $ \ v ->
        -- monadically marshal the vectors into arrays for use with CBLAS
        ioProperty $ do
            let c = cos a
            let s = sin a
            -- compute the expected and observed values
            expected <- ref n u incx v incy c s
            let observed = func n u incx v incy c s
            runTest expected observed

-- | Evidence that the native srotmg function is byte equivalent to the BLAS
-- implementation.  Parameter values that are in the range of approximately
-- +/-(epsilon/2,2/epsilon) are tested.
rotmgTest :: (Eq a, Show a)
    => String
    -> (a -> a -> a -> a -> ModGivensRot a )
    -> (a -> a -> a -> a -> IO (ModGivensRot a))
    -> Gen a
    -> TestTree
rotmgTest testname func ref gen = testProperty testname $
   forAll gen $ \ sd1 ->
   forAll gen $ \ sd2 ->
   forAll gen $ \ sx1 ->
   forAll gen $ \ sy1 ->
      -- monadically marshal the vectors into arrays for use with CBLAS
      ioProperty $ do
          -- compute the expected and observed values
          expected <- ref sd1 sd2 sx1 sy1
          let observed = func sd1 sd2 sx1 sy1
          runTest expected observed

-- | Evidence that the native srotmg function is byte equivalent to the BLAS
-- implementation.  Parameter values that are in the range of approximately
-- +/-(epsilon/2,2/epsilon) are tested.
rotmTest :: (V.Storable a, Show a, Eq a)
    => String
    -> ( a -> a -> a -> a -> ModGivensRot a)
    -> (ModGivensRot a -> Int -> V.Vector a -> Int -> V.Vector a -> Int -> (V.Vector a, V.Vector a))
    -> (ModGivensRot a -> Int -> V.Vector a -> Int -> V.Vector a -> Int -> IO (V.Vector a, V.Vector a))
    -> Gen a
    -> Gen Int
    -> TestTree
rotmTest testname mg func ref gen genInc = testProperty testname $
      forAll gen $ \ sd1 ->
      forAll gen $ \ sd2 ->
      forAll gen $ \ sx1 ->
      forAll gen $ \ (mg sd1 sd2 sx1 -> flags ) ->
      forAll (choose (1,100)) $ \ n ->
      forAll (genInc `suchThat` (/=0)) $ \ incx ->
      forAll (genInc `suchThat` (/=0)) $ \ incy ->
      forAll (choose (1,100)) $ \ mx ->
      forAll (choose (1,100)) $ \ my ->
      forAll (genNVector gen (mx+(n-1)*abs incx )) $ \ u ->
      forAll (genNVector gen (my+(n-1)*abs incy )) $ \ v ->
         ioProperty $ do
             let observed = func flags n u incx v incy
             expected <- ref flags n u incx v incy
             runTest expected observed



-- | Evidence that the native a native haskell function and a FOTRAN function
-- of the types
--
-- @
-- native :: (Int -> Vector Float -> Int -> a )
-- fortran :: (Int -> Ptr Float -> Int -> IO a )
-- @
--
-- produce byte equivalent to the results.   Vectors of length 1-10 are tested
-- having elements that are in the range of approximately (epsilon/2,2/epsilon)
iviTest :: (Eq a, Show a, V.Storable b, Show b)
        => String
        -> (Int -> V.Vector b -> Int -> a)
        -> (Int -> V.Vector b -> Int -> IO a)
        -> Gen b
        -> Gen Int
        -> TestTree
iviTest testname func funcIO gen genInc = testProperty testname $
   -- Choose the length of the vector
   forAll (choose (0,100)) $ \ n ->
   -- Randomly generate two vectors of the chosen length
   forAll (genInc) $ \ incx ->
   forAll (genNVector gen (1+(n-1)*abs incx )) $ \ u ->
   -- monadically marshal the vectors into arrays for use with CBLAS
      ioProperty $ do
          -- compute the expected and observed values
          expected <- funcIO n u incx
          let observed = func n u incx
          runTest expected observed


-- | Compare the observed and expected values and print ruman readable error
-- message.
runTest :: (Show a, Eq a) => a -> a -> IO Bool
runTest expected observed =
  if expected==observed
        then return True
        else do
            putStrLn ""
            putStrLn $ "EXPECTED: " ++ show expected
            putStrLn $ "OBSERVED: " ++ show observed
            return False


-- | Test all the random number generators for a given floating point type.
genTests :: (Random a, RealFloat a,Show a)=> String -> a -> TestTree
genTests str x = testGroup str
    [ denormalizedTest x
    , tinyTest x
    , hugeTest x
    , everydayTest x
    , largeTest x
    , smallTest x
    , testRanges x
    ]

-- | Produce evidence that the denormalized range contains only positive
-- finite, denormalized values
denormalizedTest :: ( RealFloat a,Show a)=> a -> TestTree
denormalizedTest x = testGroup "genDenormalized"
   [ testProperty "isDenormalized" $ forAll gen isDenormalized
   , testProperty "isPositive" $ forAll gen (>0)
   , testProperty "inRange" $ forAll gen (inRange (denormalizedRange x))
   , testProperty "Not well scaled wrt 1" $ forAll gen (\ z -> z+1 == max z 1 )
   ]
   where
   gen = genDenormalized x

-- | Produce evidence that the tiny range contains only positive,
-- non-denoralized, finite values that when squared are zero.
tinyTest :: (Random a, RealFloat a,Show a)=> a->TestTree
tinyTest x = testGroup "genTiny"
   [ testProperty "isPositive" $ forAll gen (>0)
   , testProperty "isNotDenormalized" $ forAll gen (not.isDenormalized)
   , testProperty "Square is zero" $ forAll gen ((==0).(**2))
   , testProperty "isNotZero" $ forAll gen (/=0)
   , testProperty "isFinite" $ forAll gen (not.isInfinite)
   , testProperty "isANumber" $ forAll gen (not.isNaN)
   , testProperty "inRange" $ forAll gen (inRange (tinyRange x))
   , testProperty "Not well scaled wrt 1" $ forAll gen (\ z -> z+1 == max z 1 )
   ]
   where
   gen = genTiny x

-- | Produce evidence that the huge numbers contain only positive,
-- normalized, finite values that when squared are infinite.
hugeTest :: (Random a, RealFloat a,Show a)=> a->TestTree
hugeTest x = testGroup "genHuge"
  [ testProperty "isPositive" $ forAll gen (>0)
  , testProperty "isNotDenormalized" $ forAll gen (not.isDenormalized)
  , testProperty "Square is Infinite" $ forAll gen (isInfinite.(**2))
  , testProperty "isNotZero" $ forAll gen (/=0)
  , testProperty "isFinite" $ forAll gen (not.isInfinite)
  , testProperty "isANumber" $ forAll gen (not.isNaN)
  , testProperty "inRange" $ forAll gen (inRange (hugeRange x))
  , testProperty "Not well scaled wrt 1" $ forAll gen (\ z -> z+1 == max z 1)
  ]
  where
  gen = genHuge x

-- | Produce evidence that the everyday numbers contain only positive,
-- normalized, finite values that are well scaled wrt unity.
everydayTest :: (Random a, RealFloat a,Show a)=> a->TestTree
everydayTest x = testGroup "genEveryday"
    [ testProperty "isPositive" $ forAll gen (>0)
    , testProperty "isNotDenormalized" $ forAll gen (not.isDenormalized)
    , testProperty "Square is Finite" $ forAll gen (not.isInfinite.(**2))
    , testProperty "Square is non-zero" $ forAll gen ((/=0).(**2))
    , testProperty "isNotZero" $ forAll gen (/=0)
    , testProperty "isFinite" $ forAll gen (not.isInfinite)
    , testProperty "isANumber" $ forAll gen (not.isNaN)
    , testProperty "inRange" $ forAll gen (inRange (everydayRange x))
    , testProperty "Well scaled wrt 1" $ forAll gen (\ z -> z+1 > max z 1 )
    ]
    where
    gen = genEveryday x

-- | Produce evidence that the large range contains positive normalized finite numbers
-- that are not well scaled wrt unity, nor are their squares finite or zero.
largeTest :: (Random a, RealFloat a,Show a)=> a->TestTree
largeTest x = testGroup "genLarge"
    [ testProperty "isPositive" $ forAll gen (>0)
    , testProperty "isNotDenormalized" $ forAll gen (not.isDenormalized)
    , testProperty "Square is Finite" $ forAll gen (not.isInfinite.(**2))
    , testProperty "Square is non-zero" $ forAll gen ((/=0).(**2))
    , testProperty "isNotZero" $ forAll gen (/=0)
    , testProperty "isFinite" $ forAll gen (not.isInfinite)
    , testProperty "isANumber" $ forAll gen (not.isNaN)
    , testProperty "inRange" $ forAll gen (inRange (largeRange x))
    , testProperty "Not Well scaled wrt 1" $ forAll gen (\ z -> z+1 == max z 1 )
    ]
    where
    gen = genLarge x

-- | Produce evidence that the small range contains positive normalized finite numbers
-- that are not well scaled wrt unity, nor are their squares finite or zero.
smallTest :: (Random a, RealFloat a,Show a)=> a->TestTree
smallTest x = testGroup "genSmall"
    [ testProperty "isPositive" $ forAll gen (>0)
    , testProperty "isNotDenormalized" $ forAll gen (not.isDenormalized)
    , testProperty "Square is Finite" $ forAll gen (not.isInfinite.(**2))
    , testProperty "Square is non-zero" $ forAll gen ((/=0).(**2))
    , testProperty "isNotZero" $ forAll gen (/=0)
    , testProperty "isFinite" $ forAll gen (not.isInfinite)
    , testProperty "isANumber" $ forAll gen (not.isNaN)
    , testProperty "inRange" $ forAll gen (inRange (smallRange x))
    , testProperty "Not Well scaled wrt 1" $ forAll gen (\ z -> z+1 == max z 1 )
    ]
    where
    gen = genSmall x

-- | Confirm that the denormalized, tiny, small, everyday, large and huge ranges
-- are adjacent and cover all the finite representable floating point numbers.
testRanges :: ( RealFloat a) => a -> TestTree
testRanges x = testGroup "Ranges cover all values" [
    testCase "includes largest finite value" $
        assertBool "largest value is excluded" ubIsLargest,
    testCase "includes smallest positive value" $
        assertBool "smallest positive value is excluded" lbIsSmallest,
    testCase "ranges are adjacent" $
        assertBool "ranges are not adjacent" coversRange
    ]
    where
    p = floatDigits x
    (m,_) = floatRange x
    ubIsLargest = isInfinite (alterFloat succ (snd $ hugeRange x))
    lbIsSmallest = (fst $ denormalizedRange x) == encodeFloat 1 (m-p)
    coversRange = areAdjacent (denormalizedRange x) (tinyRange x)
                 && areAdjacent (tinyRange x) (smallRange x)
                 && areAdjacent (smallRange x) (everydayRange x)
                 && areAdjacent (everydayRange x) (largeRange x)
                 && areAdjacent (largeRange x) (hugeRange x)

-- | Evidence that the native sdot function is byte equivalent to the CBLAS
-- implementation.  Vectors of length 1-10 are tested having elements that are
-- in the range of approximately (epsilon/2,2/epsilon)
scalTest :: (V.Storable a, Eq a, Show a)
    => String
    -> (Int -> a -> V.Vector a -> Int -> V.Vector a)
    -> (Int -> a -> V.Vector a -> Int -> IO (V.Vector a))
    -> Gen a
    -> Gen Int
    -> TestTree
scalTest testname func ref gen genInc = testProperty testname $
    -- Choose the length of the vector
    forAll (choose (1,100)) $ \ n ->
    forAll gen $ \ a ->
    -- Randomly generate a vector of the chosen length
    forAll genInc $ \ incx ->
    forAll (genNVector gen (1+(n-1)*incx )) $ \ u ->
       -- monadically marshal the vectors into arrays for use with CBLAS
       ioProperty $ do
           -- compute the expected and observed values
           expected     <- ref n a u incx
           let observed = func n a u incx
           runTest expected observed

-- | Evidence that the native scopy function is byte equivalent to the CBLAS
-- implementation.  Vectors of length 1-10 are tested having elements that are
-- in the range of approximately (epsilon/2,2/epsilon)
copyTest :: (V.Storable a, Show a, Eq a)
    => String
    -> (Int -> V.Vector a -> Int -> V.Vector a -> Int -> V.Vector a)
    -> (Int -> V.Vector a -> Int -> V.Vector a -> Int -> IO (V.Vector a))
    -> Gen a
    -> Gen Int
    -> TestTree
copyTest testname func ref gen genInc = testProperty testname $
    -- Choose the length of the vector
    forAll (choose (1,5)) $ \ n ->
    forAll (choose (1,2)) $ \ mx ->
    forAll (choose (1,2)) $ \ my ->
    -- Randomly generate a vector of the chosen length
    forAll genInc $ \ incx ->
    forAll genInc $ \ incy ->
    forAll (genNVector gen (mx+1+(n-1)*abs incx )) $ \ u ->
    forAll (genNVector gen (my+1+(n-1)*abs incy )) $ \ v ->
       -- monadically marshal the vectors into arrays for use with CBLAS
       ioProperty $ do
           -- compute the expected and observed values
           expected <- ref n u incx v incy
           let observed = func n u incx v incy
           runTest expected observed

-- | Evidence that the native saxpy function is byte equivalent to the CBLAS
-- implementation.  Vectors of length 1-10 are tested having elements that are
-- in the range of approximately (epsilon/2,2/epsilon)
axpyTest :: (V.Storable a, Eq a, Show a)
    => String
    -> (Int -> a -> V.Vector a -> Int -> V.Vector a -> Int -> V.Vector a)
    -> (Int -> a -> V.Vector a -> Int -> V.Vector a -> Int -> IO (V.Vector a))
    -> Gen a
    -> Gen Int
    -> TestTree
axpyTest testname func ref gen genInc = testProperty testname $
    -- Choose the length of the vector
    forAll (choose (1,100)) $ \ n ->
    forAll gen $ \ a ->
    -- Randomly generate a vector of the chosen length
    forAll (genInc `suchThat` (/=0))$ \ incx ->
    forAll (genInc `suchThat` (/=0)) $ \ incy ->
    forAll (choose (1,100)) $ \ mx ->
    forAll (choose (1,100)) $ \ my ->
    forAll (genNVector gen (mx+(n-1)*abs incx )) $ \ u ->
    forAll (genNVector gen (my+(n-1)*abs incy )) $ \ v ->
       ioProperty $ do
           expected <- ref n a u incx v incy
           let observed = func n a u incx v incy
           runTest expected observed
