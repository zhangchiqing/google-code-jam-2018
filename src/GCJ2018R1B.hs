{-# LANGUAGE DeriveFunctor, FlexibleContexts, FlexibleInstances,
  MultiParamTypeClasses, MultiWayIf, RecordWildCards,
  ScopedTypeVariables, TupleSections #-}

module GCJ2018R1B where

import Debug.Trace

-- import Numeric.Search
import Test.Tasty
import Test.Tasty.HUnit

import Control.Applicative
import Data.Functor.Identity
import Data.Maybe (fromJust, listToMaybe)
import Prelude hiding (init, pred)

import Data.List
import Data.Tuple

main :: IO ()
main = interact runFile

runFile :: String -> String
runFile = unlines . zipWith format [1 ..] . map (uncurry4 solve) . parse

uncurry4 f (a, b, c, d) = f a b c d

-- uncurry2 f (a, b) = f a b
parse :: String -> [(Int, Int, Int, [(Int, Int, Int)])]
parse = parse' . tail . map read . words
  where
    parse' [] = []
    parse' (r:b:c:xs) =
      (r, b, c, chunk3 (take (c * 3) xs)) : parse' (drop (c * 3) xs)

chunk3 :: [Int] -> [(Int, Int, Int)]
chunk3 [] = []
chunk3 (m:s:p:xs) = (m, s, p) : chunk3 xs

format :: Int -> Int -> String
format x y = "Case #" ++ show x ++ ": " ++ show y

solve :: Int -> Int -> Int -> [(Int, Int, Int)] -> Int
solve r b c msp = binarySearch (compute r b c msp)

compute :: Int -> Int -> Int -> [(Int, Int, Int)] -> Int -> Bool
compute r b c msp x = total >= b
  where
    total = sum $ take r $ reverse $ sort (nBits x <$> msp)

nBits :: Int -> (Int, Int, Int) -> Int
nBits x (m, s, p) = min m $ (max 0 (x - p) `div` s)

start = (1, 1, -1)

binarySearch :: (Int -> Bool) -> Int
binarySearch f = v
  where
    Just v = smallest True $ search (fromTo 1 maxBound) divForever f

bsearch :: (Int -> Bool) -> Int
bsearch f = iter f start

divide :: Int -> Int -> Int
divide a b = (a + b) `div` 2

-- divide a b = ceiling $ fromIntegral (a + b) / 2
iter :: (Int -> Bool) -> (Int, Int, Int) -> Int
iter f (low, me, high)
  | mid && (me - low <= 1) = me
  | mid = iter f (low, divide low me, me)
  | not mid && high == -1 = iter f (me, me * 2, high)
  | not mid && (me == high || high - me == 1) = high
  | not mid && high /= -1 = iter f (me, divide me high, high)
  where
    mid = f me

tests :: TestTree
tests =
  testGroup
    ""
    [ testCase "OK" $ True @?= True
    , testCase "OK" $ True @?= True
    , testCase "sample" $ do
        input <- readFile "./cases/2018R1B/in"
        otput <- readFile "./cases/2018R1B/out"
        runFile input @?= otput
    ]

runTests = defaultMain tests

-- $setup
-- All the doctests in this document assume:
--
-- >>> :set -XFlexibleContexts
-- >>> import Data.SBV
-- * Evidence
-- | The 'Evidence' datatype is similar to 'Either' , but differes in that all 'CounterEvidence' values are
--   equal to each other, and all 'Evidence' values are also
--   equal to each other. The 'Evidence' type is used to binary-searching for some predicate and meanwhile returning evidences for that.
--
-- In other words, 'Evidence' is a 'Bool' with additional information why it is 'True' or 'False'.
--
-- >>> Evidence "He owns the gun" == Evidence "He was at the scene"
-- True
-- >>> Evidence "He loved her" == CounterEvidence "He loved her"
-- False
data Evidence a b
  = CounterEvidence a
  | Evidence b
  deriving (Show, Read, Functor)

instance Eq (Evidence b a) where
  CounterEvidence _ == CounterEvidence _ = True
  Evidence _ == Evidence _ = True
  _ == _ = False

instance Ord (Evidence b a) where
  CounterEvidence _ `compare` CounterEvidence _ = EQ
  Evidence _ `compare` Evidence _ = EQ
  CounterEvidence _ `compare` Evidence _ = GT
  Evidence _ `compare` CounterEvidence _ = LT

instance Applicative (Evidence e) where
  pure = Evidence
  CounterEvidence e <*> _ = CounterEvidence e
  Evidence f <*> r = fmap f r

instance Monad (Evidence e) where
  return = Evidence
  CounterEvidence l >>= _ = CounterEvidence l
  Evidence r >>= k = k r

-- | 'evidence' = 'Evidence' 'undefined'. We can use this combinator to look up for some 'Evidence',
-- since all 'Evidence's are equal.
evidence :: Evidence a b
evidence = Evidence undefined

-- | 'counterEvidence' = 'CounterEvidence' 'undefined'. We can use this combinator to look up for any 'CounterEvidence',
-- since all 'CounterEvidence's are equal.
counterEvidence :: Evidence a b
counterEvidence = CounterEvidence undefined

-- * Search range
-- | The @Range k lo  k' hi@ represents the search result that @pred x == k@ for @lo <= x <= hi@.
-- The 'Range' type also holds the evidences for the lower and the upper boundary.
data Range b a = Range
  { loKey :: b
  , loVal :: a
  , hiKey :: b
  , hiVal :: a
  } deriving (Show, Read, Eq, Ord)

-- | The (possibly infinite) lists of candidates for lower and upper bounds from which the search may be started.
type SearchRange a = ([a], [a])

-- | Set the lower and upper boundary from those available from the candidate lists.
-- From the pair of list, the @initializeSearchM@ tries to find the first @(lo, hi)@
-- such that @pred lo /= pred hi@, by the breadth-first search.
initializeSearchM ::
     (Monad m, Eq b) => SearchRange a -> (a -> m b) -> m [Range b a]
initializeSearchM (lo:los, hi:his) pred0 = do
  pLo <- pred0 lo
  pHi <- pred0 hi
  let pop (p, x, []) = return (p, x, [])
      pop (p, _, x2:xs) = do
        p2 <- pred0 x2
        return (p2, x2, xs)
      go pez1@(p1, x1, xs1) pez2@(p2, x2, xs2)
        | p1 /= p2 = return [Range p1 x1 p1 x1, Range p2 x2 p2 x2]
        | null xs1 && null xs2 = return [Range p1 x1 p2 x2]
        | otherwise = do
          pez1' <- pop pez1
          pez2' <- pop pez2
          go pez1' pez2'
  go (pLo, lo, los) (pHi, hi, his)
initializeSearchM _ _ = return []

-- | Start binary search from between 'minBound' and 'maxBound'.
minToMax :: Bounded a => SearchRange a
minToMax = ([minBound], [maxBound])

-- | Start binary search from between the given pair of boundaries.
fromTo :: a -> a -> SearchRange a
fromTo x y = ([x], [y])

-- | Exponentially search for lower boundary from @[-1, -2, -4, -8, ...]@, upper boundary from @[0, 1, 2, 4, 8, ...]@.
-- Move on to the binary search once the first @(lo, hi)@ is found
-- such that @pred lo /= pred hi@.
exponential :: Num a => SearchRange a
exponential = (iterate (* 2) (-1), 0 : iterate (* 2) 1)

-- | Lower boundary is 1, upper boundary is from @[2, 4, 8, 16, ...]@.
positiveExponential :: Num a => SearchRange a
positiveExponential = ([1], iterate (* 2) 2)

-- | Lower boundary is 0, search upper boundary is from @[1, 2, 4, 8, ...]@.
nonNegativeExponential :: Num a => SearchRange a
nonNegativeExponential = ([0], iterate (* 2) 1)

-- | Lower boundary is @[0.5, 0.25, 0.125, ...]@, upper boundary is from @[1, 2, 4, 8, ...]@.
positiveFractionalExponential :: Fractional a => SearchRange a
positiveFractionalExponential = (iterate (/ 2) 0.5, iterate (* 2) 1)

-- | Lower boundary is from @[-2, -4, -8, -16, ...]@, upper boundary is -1.
negativeExponential :: Num a => SearchRange a
negativeExponential = (iterate (* 2) (-2), [-1])

-- | Lower boundary is from @[-1, -2, -4, -8, ...]@, upper boundary is -0.
nonPositiveExponential :: Num a => SearchRange a
nonPositiveExponential = (iterate (* 2) (-1), [0])

-- | Lower boundary is @[-1, -2, -4, -8, ...]@, upper boundary is from @[-0.5, -0.25, -0.125, ...]@.
negativeFractionalExponential :: Fractional a => SearchRange a
negativeFractionalExponential = (iterate (* 2) (-1), iterate (/ 2) (-0.5))

-- * Splitters
-- | The type of function that returns a value between @(lo, hi)@ as long as one is available.
type Splitter a = a -> a -> Maybe a

-- | Perform split forever, until we cannot find a mid-value because @hi-lo < 2@.
-- This splitter assumes that the arguments are Integral, and uses the `div` funtion.
--
-- Note that our dividing algorithm always find the mid value for any  @hi-lo >= 2@.
--
-- >>> prove $ \x y -> (y .>= x+2 &&& x+2 .> x) ==> let z = (x+1) `sDiv` 2 + y `sDiv` 2  in x .< z &&& z .< (y::SInt32)
-- Q.E.D.
divForever :: Integral a => Splitter a
divForever lo hi =
  let mid = (lo + 1) `div` 2 + hi `div` 2
   in if lo == mid || mid == hi
        then Nothing
        else Just mid

-- | Perform splitting until @hi - lo <= eps@.
divTill :: Integral a => a -> Splitter a
divTill eps lo hi
  | hi - lo <= eps = Nothing
  | otherwise = divForever lo hi

-- | Perform split forever, until we cannot find a mid-value due to machine precision.
-- This one uses `(/)` operator.
divideForever :: (Eq a, Fractional a) => Splitter a
divideForever lo hi =
  let mid = lo / 2 + hi / 2
   in if lo == mid || mid == hi
        then Nothing
        else Just mid

-- | Perform splitting until @hi - lo <= eps@.
divideTill :: (Ord a, Fractional a) => a -> Splitter a
divideTill eps lo hi
  | hi - lo <= eps = Nothing
  | otherwise = divideForever lo hi

-- * Searching
-- | Perform search over pure predicates. The monadic version of this is 'searchM'.
search :: (Eq b) => SearchRange a -> Splitter a -> (a -> b) -> [Range b a]
search init0 split0 pred0 =
  runIdentity $ searchM init0 split0 (Identity . pred0)

-- | Mother of all search variations.
--
-- 'searchM' keeps track of the predicates found, so that it works well with the 'Evidence' type.
searchM ::
     forall a m b. (Functor m, Monad m, Eq b)
  => SearchRange a
  -> Splitter a
  -> (a -> m b)
  -> m [Range b a]
searchM init0 split0 pred0 = do
  ranges0 <- initializeSearchM init0 pred0
  go ranges0
  where
    go :: [Range b a] -> m [Range b a]
    go (r1@(Range p0 lo1 p1 hi1):r2@(Range p2 lo2 p3 hi2):rest) =
      case split0 hi1 lo2 of
        Nothing -> (r1 :) <$> go (r2 : rest)
        Just mid1 -> do
          pMid <- pred0 mid1
          if | p1 == pMid -> go $ (Range p0 lo1 pMid mid1) : r2 : rest
             | p2 == pMid -> go $ r1 : (Range pMid mid1 p3 hi2) : rest
             | otherwise -> go $ r1 : (Range pMid mid1 pMid mid1) : r2 : rest
    go xs = return xs

-- * Postprocess
-- | Look up for the first 'Range' with the given predicate.
lookupRanges :: (Eq b) => b -> [Range b a] -> Maybe (Range b a)
lookupRanges k [] = Nothing
lookupRanges k (r@Range {..}:rs)
  | loKey == k = Just r
  | otherwise = lookupRanges k rs

-- | Pick up the smallest @a@ that satisfies @pred a == b@.
smallest :: (Eq b) => b -> [Range b a] -> Maybe a
smallest b rs = loVal <$> lookupRanges b rs

-- | Pick up the largest @a@ that satisfies @pred a == b@.
largest :: (Eq b) => b -> [Range b a] -> Maybe a
largest b rs = hiVal <$> lookupRanges b rs

-- | Get the content of the evidence for the smallest @a@ which satisfies @pred a@.
evidenceForSmallest :: [Range (Evidence b1 b2) a] -> Maybe b2
evidenceForSmallest rs = listToMaybe [e | Evidence e <- map loKey rs]

-- | Get the content of the evidence for the largest @a@ which satisfies @pred a@.
evidenceForLargest :: [Range (Evidence b1 b2) a] -> Maybe b2
evidenceForLargest rs = listToMaybe [e | Evidence e <- map hiKey rs]

-- | Get the content of the counterEvidence for the smallest @a@ which does not satisfy @pred a@.
counterEvidenceForSmallest :: [Range (Evidence b1 b2) a] -> Maybe b1
counterEvidenceForSmallest rs =
  listToMaybe [e | CounterEvidence e <- map loKey rs]

-- | Get the content of the counterEvidence for the largest @a@ which does not satisfy @pred a@.
counterEvidenceForLargest :: [Range (Evidence b1 b2) a] -> Maybe b1
counterEvidenceForLargest rs =
  listToMaybe [e | CounterEvidence e <- map hiKey rs]
