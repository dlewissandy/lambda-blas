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
        [ dotTest "sdot" sdot (elements [-5..5])
        , sdsdotTest "sdsdot" sdsdot (elements [-5..5])
        , rotgTest "srotg" genNiceFloat Fortran.srotg srotg
        , rotgTest "drotg" genNiceDouble Fortran.drotg drotg
        , srotmgTest "srotmg" srotmg
        , srotmTest "srotm" srotm (elements [-5..5] `suchThat` (/=0))
        , iviTest "sasum" sasum (Fortran.sasum) genNiceFloat (elements [1..5])
        , iviTest "snrm2" snrm2 (Fortran.snrm2) genNiceFloat (elements [1..5])
        , iviTest "dnrm2" dnrm2 (Fortran.dnrm2) genNiceDouble (elements [1..5])
        , iviTest "isamax" (\ n u incx -> succ $ isamax n u incx ) (Fortran.isamax) genNiceFloat (elements [1..5])
        , sscalTest "sscal" sscal (elements [1..5])
        , scopyTest "scopy" scopy (elements [-5..5])
        , sswapTest "sswap" sswap (elements [-5..5])
        , srotTest "srot" srot (elements [-5,5])
        , saxpyTest "saxpy" saxpy (elements [-5,5])
        ]
    ]

-- | Evidence that the native sdot function is byte equivalent to the CBLAS
-- implementation.  Vectors of length 1-10 are tested having elements that are
-- in the range of approximately (epsilon/2,2/epsilon)
dotTest :: String
         -> (Int -> V.Vector Float -> Int -> V.Vector Float -> Int -> Float)
         -> Gen Int
         -> TestTree
dotTest testname func genInc = testProperty testname $
    -- Choose the length of the vector
    forAll (choose (1,100)) $ \ n ->
    -- Randomly generate two vectors of the chosen length
    forAll (genInc) $ \ incx ->
    forAll (genInc) $ \ incy ->
    forAll (genNVector genNiceFloat (1+(n-1)*abs incx )) $ \ u ->
    forAll (genNVector genNiceFloat (1+(n-1)*abs incy )) $ \ v ->
       -- monadically marshal the vectors into arrays for use with CBLAS
       ioProperty $ do
           -- compute the expected and observed values
           expected <- Fortran.sdot n u incx v incy
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
sswapTest :: String
        -> (Int ->  V.Vector Float -> Int -> V.Vector Float -> Int -> (V.Vector Float, V.Vector Float))
        -> Gen Int
        -> TestTree
sswapTest testname func genInc = testProperty testname $
   -- Choose the length of the vector.   Vectors will have a length of (1+(n-1)*abs inc)+m)
   forAll (choose (1,5)) $ \ n ->
   forAll (choose (0,2)) $ \ mx ->
   forAll (choose (0,2)) $ \ my ->
   -- Randomly generate two vectors of the chosen length
   forAll (genInc `suchThat` (/=0)) $ \ incx ->
   forAll (genInc `suchThat` (/=0)) $ \ incy ->
   forAll (pure $ V.take (mx+1+(n-1)*abs incx) $ V.fromList [1..100]) $ \ u -> -- (genNVector genNiceFloat (mx+1+(n-1)*abs incx )) $ \ u ->
   forAll (pure $ V.take (my+1+(n-1)*abs incy) $ V.fromList [-100..0]) $ \ v -> -- (genNVector genNiceFloat (my+1+(n-1)*abs incy )) $ \ v ->

      -- monadically marshal the vectors into arrays for use with CBLAS
      ioProperty $ do
          -- compute the expected and observed values
          expected <- Fortran.sswap n u incx v incy
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
srotTest :: String -> ( Int -> V.Vector Float -> Int -> V.Vector Float -> Int -> Float -> Float -> (V.Vector Float, V.Vector Float)) -> Gen Int -> TestTree
srotTest testname func genInc = testProperty testname $
    -- Choose the length of the vector
    forAll (choose (1,100)) $ \ n ->
    forAll genNiceFloat $ \ a ->
    -- Randomly generate two vectors of the chosen length
    forAll (genInc `suchThat` (/=0)) $ \ incx ->
    forAll (genInc `suchThat` (/=0)) $ \ incy ->
    forAll (genNVector genNiceFloat (1+(n-1)*abs incx )) $ \ u ->
    forAll (genNVector genNiceFloat (1+(n-1)*abs incy )) $ \ v ->
        -- monadically marshal the vectors into arrays for use with CBLAS
        ioProperty $ do
            let c = cos a
            let s = sin a
            -- compute the expected and observed values
            expected <- Fortran.srot n u incx v incy c s
            let observed = func n u incx v incy c s
            runTest expected observed

-- | Evidence that the native srotmg function is byte equivalent to the BLAS
-- implementation.  Parameter values that are in the range of approximately
-- +/-(epsilon/2,2/epsilon) are tested.
srotmgTest :: String -> (Float -> Float -> Float -> Float -> ModGivensRot Float ) -> TestTree
srotmgTest testname func = testProperty testname $
   forAll genNiceFloat $ \ sd1 ->
   forAll genNiceFloat $ \ sd2 ->
   forAll genNiceFloat $ \ sx1 ->
   forAll genNiceFloat $ \ sy1 ->
      -- monadically marshal the vectors into arrays for use with CBLAS
      ioProperty $ do
          -- compute the expected and observed values
          expected <- Fortran.srotmg sd1 sd2 sx1 sy1
          let observed = func sd1 sd2 sx1 sy1
          runTest expected observed

-- | Evidence that the native srotmg function is byte equivalent to the BLAS
-- implementation.  Parameter values that are in the range of approximately
-- +/-(epsilon/2,2/epsilon) are tested.
srotmTest :: String -> (ModGivensRot Float -> Int -> V.Vector Float -> Int -> V.Vector Float -> Int -> (V.Vector Float, V.Vector Float ) ) -> Gen Int -> TestTree
srotmTest testname func genInc = testProperty testname $
      forAll genNiceFloat $ \ sd1 ->
      forAll genNiceFloat $ \ sd2 ->
      forAll genNiceFloat $ \ sx1 ->
      forAll genNiceFloat $ \ (srotmg sd1 sd2 sx1 -> flags ) ->
      forAll (choose (1,100)) $ \ n ->
      forAll (genInc `suchThat` (/=0)) $ \ incx ->
      forAll (genInc `suchThat` (/=0)) $ \ incy ->
      forAll (choose (1,100)) $ \ mx ->
      forAll (choose (1,100)) $ \ my ->
      forAll (genNVector genNiceFloat (mx+(n-1)*abs incx )) $ \ u ->
      forAll (genNVector genNiceFloat (my+(n-1)*abs incy )) $ \ v ->
         ioProperty $ do
             let observed = func flags n u incx v incy
             expected <- Fortran.srotm flags n u incx v incy
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
sscalTest :: String
         -> (Int -> Float -> V.Vector Float -> Int -> V.Vector Float)
         -> Gen Int
         -> TestTree
sscalTest testname func genInc = testProperty testname $
    -- Choose the length of the vector
    forAll (choose (1,100)) $ \ n ->
    forAll genNiceFloat $ \ a ->
    -- Randomly generate a vector of the chosen length
    forAll genInc $ \ incx ->
    forAll (genNVector genNiceFloat (1+(n-1)*incx )) $ \ u ->
       -- monadically marshal the vectors into arrays for use with CBLAS
       ioProperty $ do
           -- compute the expected and observed values
           expected <- Fortran.sscal n a u incx
           let observed = func n a u incx
           runTest expected observed

-- | Evidence that the native scopy function is byte equivalent to the CBLAS
-- implementation.  Vectors of length 1-10 are tested having elements that are
-- in the range of approximately (epsilon/2,2/epsilon)
scopyTest :: String
         -> (Int -> V.Vector Float -> Int -> V.Vector Float -> Int -> V.Vector Float)
         -> Gen Int
         -> TestTree
scopyTest testname func genInc = testProperty testname $
    -- Choose the length of the vector
    forAll (choose (1,5)) $ \ n ->
    forAll (choose (1,2)) $ \ mx ->
    forAll (choose (1,2)) $ \ my ->
    -- Randomly generate a vector of the chosen length
    forAll genInc $ \ incx ->
    forAll genInc $ \ incy ->
    forAll (genNVector genNiceFloat (mx+1+(n-1)*abs incx )) $ \ u ->
    forAll (genNVector genNiceFloat (my+1+(n-1)*abs incy )) $ \ v ->
       -- monadically marshal the vectors into arrays for use with CBLAS
       ioProperty $ do
           -- compute the expected and observed values
           expected <- Fortran.scopy n u incx v incy
           let observed = func n u incx v incy
           runTest expected observed

-- | Evidence that the native saxpy function is byte equivalent to the CBLAS
-- implementation.  Vectors of length 1-10 are tested having elements that are
-- in the range of approximately (epsilon/2,2/epsilon)
saxpyTest :: String
         -> (Int -> Float -> V.Vector Float -> Int -> V.Vector Float -> Int -> V.Vector Float)
         -> Gen Int
         -> TestTree
saxpyTest testname func genInc = testProperty testname $
    -- Choose the length of the vector
    forAll (choose (1,100)) $ \ n ->
    forAll genNiceFloat $ \ a ->
    -- Randomly generate a vector of the chosen length
    forAll (genInc `suchThat` (/=0))$ \ incx ->
    forAll (genInc `suchThat` (/=0)) $ \ incy ->
    forAll (choose (1,100)) $ \ mx ->
    forAll (choose (1,100)) $ \ my ->
    forAll (genNVector genNiceFloat (mx+(n-1)*abs incx )) $ \ u ->
    forAll (genNVector genNiceFloat (my+(n-1)*abs incy )) $ \ v ->
       ioProperty $ do
           expected <- Fortran.saxpy n a u incx v incy
           let observed = func n a u incx v incy
           runTest expected observed
