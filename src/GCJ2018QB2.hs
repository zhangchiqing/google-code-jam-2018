module GCJ2018QB2 where

import Debug.Trace
import qualified GCJ2018QB as QB
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Data.List

main :: IO ()
main = interact runFile

runFile :: String -> String
runFile = unlines . zipWith format [1 ..] . map (uncurry2 solve) . parse

uncurry2 f (a, b) = f a b

parse :: String -> [(Int, [Int])]
parse = parse' . map read . tail . words
  where
    parse' [] = []
    parse' (n:xs) =
      let (vs, xs') = splitAt n xs
       in (n, vs) : parse' xs'

format :: Int -> String -> String
format x y = "Case #" ++ show x ++ ": " ++ y

solve' :: [Int] -> String
solve' vs = solve (length vs) vs

solve :: Int -> [Int] -> String
solve n vs =
  case find notEq (zip3 [0 ..] vs' troubleVs) of
    Nothing -> "OK"
    Just (i, _, _) -> show i
  where
    (evens, odds) = breakIntoEvenAndOdds vs
    odds' = sort odds
    evens' = sort evens
    troubleVs = concat $ transpose [evens', odds']
    vs' = sort vs
    notEq (_, x, y) = x /= y

breakIntoEvenAndOdds :: [Int] -> ([Int], [Int])
breakIntoEvenAndOdds = foldr (\x xs -> (x : snd xs, fst xs)) ([], [])

-- breakIntoEvenAndOdds :: [Int] -> ([Int], [Int])
-- breakIntoEvenAndOdds xs = (merge es, merge os)
--   where
--     (es, os) = partition (even . fst) $ zip [0 ..] xs
--     merge = snd . unzip
-- mergeWith :: [Int] -> [Int] -> [Int]
-- mergeWith xs [] = xs
-- mergeWith [] ys = ys
-- mergeWith (e:es) (o:os) = e : o : mergeWith es os
tests :: TestTree
tests =
  testGroup
    ""
    [ testCase "OK" $ True @?= True
    , testCase "OK" $ solve 1 [6, 5, 6] @?= "0"
    , testCase "OK" $ solve 1 [5, 6, 5] @?= "1"
    , testCase "OK" $ solve 1 [5, 6, 5, 6] @?= "1"
    , testCase "OK" $ solve 1 [6, 5, 6, 7] @?= "0"
    , testCase "sample" $ do
        input <- readFile "./cases/2018QB/in"
        otput <- readFile "./cases/2018QB/out"
        runFile input @?= otput
    , testProperty
        "QB.solve == QB2.solve"
        (\xs -> solve' (xs :: [Int]) == QB.solve' xs)
    ]

runTests = defaultMain tests
