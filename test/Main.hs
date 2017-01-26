-- | This test suite performs unit tests to evidence the correctness of
-- the native haskell implementations of BLAS subroutines.
module Main( main ) where

import qualified Data.Vector.Storable as V
import Foreign.Marshal.Array
import System.Random
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
-- | This package
import Numerical.BLAS.Single
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
    , nativeTest     -- Confirm that the native sdot function is byte equivalent to the CBLAS implementation
    , naiveTest      -- Confirm that the naive dot product function is equivalent to the CBLAS implementation
    ]

-- | Evidence that the naive sdot function is byte equivalent to the CBLAS
-- implementation.  Vectors of length 1-10 are tested having elements that are
-- in the range of approximately (epsilon/2,2/epsilon).
naiveTest :: TestTree
naiveTest = dotTest "sdot_zip" (\ _ u _ v _ -> sdot_zip u v) (return 1)

-- | Evidence that the native sdot function is byte equivalent to the CBLAS
-- implementation.  Vectors of length 1-10 are tested having elements that are
-- in the range of approximately (epsilon/2,2/epsilon).
nativeTest :: TestTree
nativeTest = dotTest "sdot" sdot (elements [-1,1])

-- | Evidence that the native sdot function is byte equivalent to the CBLAS
-- implementation.  Vectors of length 1-10 are tested having elements that are
-- in the range of approximately (epsilon/2,2/epsilon)
dotTest :: String
         -> (Int -> V.Vector Float -> Int -> V.Vector Float -> Int -> Float)
         -> Gen Int
         -> TestTree
dotTest testname func genInc = testProperty testname $
    -- Choose the length of the vector
    forAll (choose (1,10)) $ \ n ->
    -- Randomly generate two vectors of the chosen length
    forAll (genNVector (genFloat genEveryday) n) $ \ u ->
    forAll (genNVector (genFloat genEveryday) n) $ \ v ->
    forAll (genInc) $ \ incx ->
    forAll (genInc) $ \ incy ->
       -- monadically marshal the vectors into arrays for use with CBLAS
       ioProperty $
       withArray (V.toList u) $ \ us ->
       withArray (V.toList v) $ \ vs -> do
           -- compute the expected and observed values
           expected <- Fortran.sdot n us incx vs incy
           let observed = func n u incx v incy
           runTest expected observed
      where
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
