-- | This module provides random number generators for floating point numbers
-- and vectors thereof.    Generators for a variety of different ranges are
-- provided to facilitate testing of important corner cases (e.g. Infinites,
-- NaN, denormalized values, etc.).
{-# LANGUAGE RankNTypes #-}
module Gen
   ( -- * Generators
   genFloat
   , genDouble
   , genRealFloat
   , genDenormalized
   , genNegative
   , genTiny
   , genSmall
   , genLarge
   , genHuge
   , genEveryday
   , genVector
   , genNVector
   -- * Ranges
   , denormalizedRange
   , tinyRange
   , smallRange
   , everydayRange
   , largeRange
   , hugeRange
   -- * Helper functions
   , inRange
   , alterFloat
   , areAdjacent
   ) where

import System.Random
import Test.QuickCheck.Gen
import Data.Vector.Storable

-- | Given a generator for the elements, construct a random vector of length
-- between 1 and 1000.
genVector :: (Storable a) => Gen a -> Gen (Vector a)
genVector gen = do
    n<- choose (1,1000)
    genNVector gen n

-- | Given a generator for the elements, construct a
-- random vector of the given length
genNVector :: (Storable a) => Gen a -> Int -> Gen (Vector a)
genNVector gen n = vectorOf n gen >>= return.fromList

-- | Given a random generator for a floating point type, generate a random
-- Float value.
genFloat :: (forall a. (RealFloat a, Random a)=> a -> Gen a) -> Gen Float
genFloat gen = gen (1::Float)

-- | Given a random generator for a floating point type, generate a random
-- Double value.
genDouble :: (forall a. (RealFloat a, Random a)=> a -> Gen a) -> Gen Double
genDouble gen = gen (1::Double)

-- | Generate a random floating value.  Nan and infinities will occur
-- approximately 1% of the time.
genRealFloat :: (Random a, RealFloat a) => a -> Gen a
genRealFloat x = frequency
    [(495,genNonNegative x)
    ,(495,genNegative (genPositive x))
    ,(4, return $ 1/0)  -- infinity
    ,(4, return (-1/0)) -- neg infinity
    ,(2, return (0/0))  -- NaN
    ]

-- | Given a generator for a real float, return a generator that
-- negates the values.
genNegative :: (RealFloat a) => Gen a -> Gen a
genNegative gen = gen >>= return . negate

-- | Generate a random positive real float value.
genPositive :: (Random a, RealFloat a) => a -> Gen a
genPositive x = frequency
    [(50,genEveryday x)
    ,(5,genDenormalized x)
    ,(10,genTiny x)
    ,(10,genLarge x)
    ,(10,genSmall x)
    ,(10,genHuge x)
    ]

-- | Generate a random non-negative real float value.  Zero will
-- occur approximately 5% of the time.
genNonNegative :: (Random a, RealFloat a) => a -> Gen a
genNonNegative x = frequency
    [(5,return 0)
    ,(95,genPositive x)
    ]

-- | Generate a positive denormalized number of the given type.
-- note: The built in haskell random instance for float will not
-- reliably generate denormalized numbers (it will randomly generate
-- zeros).   As a work around, we use the choose to generate
-- the integer significand and encode float to construct the returned result.
genDenormalized :: (RealFloat a) => a -> Gen a
genDenormalized x = choose ( 1, r^(p-1)-1) >>= \ s ->
    return $ encodeFloat (s*r^(2::Integer)) (m-p-2)
    where
    ( m , _ ) = floatRange x
    r = floatRadix x
    p = floatDigits x
{-# SPECIALIZE genDenormalized :: Double -> Gen Double #-}
{-# SPECIALIZE genDenormalized :: Float -> Gen Float #-}

-- | Generate a random floating point number drawn from a
-- uniform distribution between  approximately epilon and
-- 1/epsilon.
genEveryday :: (Random a, RealFloat a)=> a -> Gen a
genEveryday x = choose $ everydayRange x
{-# SPECIALIZE genEveryday :: Double -> Gen Double #-}
{-# SPECIALIZE genEveryday :: Float -> Gen Float #-}

-- | Generate a floating number between approximately
-- 1/epsilon and the largest squarable number of the
-- given type.
genLarge :: (Random a, RealFloat a)=> a -> Gen a
genLarge x = choose $ largeRange x
{-# SPECIALIZE genLarge :: Double -> Gen Double #-}
{-# SPECIALIZE genLarge :: Float -> Gen Float #-}

-- | Generate a floating number between largest squarable number of
-- the given type and the largest finite representable value of
-- the type.
genHuge :: (Random a, RealFloat a)=> a -> Gen a
genHuge x = choose $ hugeRange x
{-# SPECIALIZE genHuge :: Double -> Gen Double #-}
{-# SPECIALIZE genHuge :: Float -> Gen Float #-}

-- | Generate a floating number between approximately epsilon/2 and
-- the smallest squareable non-denormalized number of the given type.
genSmall :: (Random a, RealFloat a) => a -> Gen a
genSmall x = choose $ smallRange x
{-# SPECIALIZE genSmall :: Double -> Gen Double #-}
{-# SPECIALIZE genSmall :: Float -> Gen Float #-}

-- | Generate a random tiny number.   Tiny numbers are not denormalized, but
-- they are so small that when they are squared, the result rounds to zero.
genTiny :: (Random a, RealFloat a) => a -> Gen a
genTiny x = choose (tinyRange x)
{-# SPECIALIZE genTiny :: Float -> Gen Float #-}
{-# SPECIALIZE genTiny :: Double -> Gen Double #-}

-- Compute the range of non-denormalized numbers that when squared are equal to
-- zero.  Assumes a radix of 2.
tinyRange::(RealFloat a)=> a -> (a,a)
{-# INLINE tinyRange #-}
tinyRange x = (minA, maxA)
  where
  minA = alterFloat succ $ snd $ denormalizedRange x
  maxA | (m-p) `mod` 2 /= 0 = encodeFloat 1 ((m-p-1) `div` 2)
       | otherwise          = let (s,e) = decodeFloat $ (encodeFloat 1 ((m-p-2) `div` 2)) * sqrt (2::Double)
                              in  encodeFloat (s-1) e
  p = floatDigits x
  (m,_)= floatRange x
{-# SPECIALIZE tinyRange :: Double -> (Double,Double) #-}
{-# SPECIALIZE tinyRange :: Float -> (Float,Float) #-}

-- the range of denormalized numbers of a given type
denormalizedRange :: (RealFloat a)=> a -> (a,a)
{-# INLINE denormalizedRange #-}
denormalizedRange x = ( encodeFloat 1 (m-p)
                      , encodeFloat (r^(p-1)-1) (m-p))
    where
    r = floatRadix x
    p = floatDigits x
    (m,_) = floatRange x
{-# SPECIALIZE denormalizedRange :: Double -> (Double,Double) #-}
{-# SPECIALIZE denormalizedRange :: Float -> (Float,Float) #-}

-- Huge numbers are infinte when squared.  Return the range of the huge
-- numbers
hugeRange :: (RealFloat a)=> a -> (a,a)
{-# INLINE hugeRange #-}
hugeRange x = ( minA, maxA )
    where
    maxA = encodeFloat (r^p -1) (m-p)
    minA = encodeFloat 1 (m `div` 2)
    r = floatRadix x
    p = floatDigits x
    (_,m) = floatRange x
{-# SPECIALIZE hugeRange :: Double -> (Double,Double) #-}
{-# SPECIALIZE hugeRange :: Float -> (Float,Float) #-}

-- | Everyday range numbers are numbers that are between approximately
-- epsilon and 1/epsilon.
everydayRange :: (RealFloat a)=> a -> (a,a)
{-# INLINE everydayRange #-}
everydayRange x = ( encodeFloat 1 minExp
    , encodeFloat (r^p-1) 0 )
    where
    r = floatRadix x
    p = floatDigits x
    minExp = negate $ p
{-# SPECIALIZE everydayRange :: Double -> (Double,Double) #-}
{-# SPECIALIZE everydayRange :: Float -> (Float,Float) #-}

-- | Large range numbers are numbers that are between the everyday
-- numbers and huge numbers.
largeRange :: (RealFloat a)=> a -> (a,a)
{-# INLINE largeRange #-}
largeRange x = ( encodeFloat (succ s0) e0, encodeFloat (r^p -1) (m `div` 2 - p))
    where
    (_,m)=floatRange x
    p = floatDigits x
    r = floatRadix x
    (s0,e0) = decodeFloat $ snd $ everydayRange x
{-# SPECIALIZE largeRange :: Double -> (Double,Double) #-}
{-# SPECIALIZE largeRange :: Float -> (Float,Float) #-}

-- | Small range numbers are numbers that are between the tiny
-- numbers and everyday numbers.
smallRange :: (RealFloat a)=> a -> (a,a)
{-# INLINE smallRange #-}
smallRange x = ( encodeFloat (succ s0) e0, maxA)
    where
    p=floatDigits x
    (s0,e0) = decodeFloat $ snd $ tinyRange x
    maxA = fst (everydayRange x)-encodeFloat 1 (-2*p)
{-# SPECIALIZE smallRange :: Double -> (Double,Double) #-}
{-# SPECIALIZE smallRange :: Float -> (Float,Float) #-}

-- | Test if an element is in a given range of values.   This predicate
-- is used by the denormalizedTest, tinyTest, smallTest, everydayTest, largeTest,
-- and hugeTest to confirm membership.
inRange :: (Ord a) => (a,a) -> a -> Bool
inRange (lb,ub) x = x>=lb && x<=ub

-- | alter a float by applying an integer function to the significand.  This
-- function is used to perform adjacency tests.
alterFloat :: (RealFloat a)=> (Integer->Integer) -> a -> a
alterFloat f x = let (s,e) = decodeFloat x
                 in  encodeFloat (f s) e


-- | Test to see if two ranges are adjacent
areAdjacent :: (RealFloat a) => (a,a) -> (a,a) -> Bool
areAdjacent (_,ub) (lb,_) = alterFloat succ ub == lb
