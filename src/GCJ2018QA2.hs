module GCJ2018QA2 where

import Debug.Trace
import qualified GCJ2018QA as QA
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Data.List (elemIndices, findIndices)

main :: IO ()
main = interact runFile

runFile :: String -> String
runFile = unlines . zipWith format [1 ..] . map (uncurry2 solve) . parse

uncurry2 f (a, b) = f a b

parse :: String -> [(Int, String)]
parse = parse' . tail . words
  where
    parse' [] = []
    parse' (n:r:xs) = (read n, r) : parse' xs

format :: Int -> String -> String
format x y = "Case #" ++ show x ++ ": " ++ y

solve' :: String -> String
solve' p = solve (length p) p

solve :: Int -> String -> String
solve d p =
  case swapping d (length p) p 0 of
    Nothing -> "IMPOSSIBLE"
    Just total -> show total

swapping :: Int -> Int -> String -> Int -> Maybe Int
swapping d cp p total
  | isPGood p d = Just total
  | otherwise =
    case findAndSwapFarRightCSToSC cp p of
      Nothing -> Nothing
      Just p' -> swapping d cp p' (total + 1)

isPGood p d = computeD p <= d

-- computeD :: String -> Int
-- computeD xs = iter xs (0, 1)
--   where
--     iter [] (total, current) = total
--     iter ('S':xs) (total, current) = iter xs (total + current, current)
--     iter ('C':xs) (total, current) = iter xs (total, current * 2)
computeD :: String -> Int
computeD = fst . foldl calc (0, 1)
  where
    calc (total, current) 'S' = (total + current, current)
    calc (total, current) 'C' = (total, current * 2)

findAndSwapFarRightCSToSC :: Int -> String -> Maybe String
findAndSwapFarRightCSToSC n p =
  if null css
    then Nothing
    else Just $ swapCSToSC (last css)
  where
    css = filter isCS $ zip3 [0 ..] p (drop 1 p)
    isCS (_, 'C', 'S') = True
    isCS _ = False
    swapCSToSC (i, _, _) =
      let (s1, s2) = splitAt i p
       in s1 ++ "SC" ++ drop 2 s2

tests :: TestTree
tests =
  testGroup
    ""
    [ testCase "OK" $ True @?= True
    , testCase "sample" $ do
        input <- readFile "./cases/2018QA/in"
        otput <- readFile "./cases/2018QA/out"
        runFile input @?= otput
    , testProperty "refactor" $
      forAll csString $ \s ->
        QA.findAndSwapFarRightCSToSC (length s) s ==
        findAndSwapFarRightCSToSC (length s) s
    -- , testProperty "refactor" $ forAll csString $ \s -> QA.solve' s == solve' s
    ]

csString :: Gen String
csString = listOf $ elements "CS"

runTests = defaultMain tests
