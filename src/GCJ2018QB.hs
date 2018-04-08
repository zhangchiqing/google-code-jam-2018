module GCJ2018QB where

import Debug.Trace
import Test.Tasty
import Test.Tasty.HUnit

import Data.List (find)

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

solve :: Int -> [Int] -> String
solve n vs = findTrouble $ troubleSort vs

findTrouble :: [Int] -> String
findTrouble xs =
  case maybeFound of
    Nothing -> "OK"
    Just (i, _) -> show i
  where
    isTrouble (i, (x, y)) = x > y
    maybeFound = find isTrouble $ zip [0 ..] $ zip xs $ drop 1 xs

troubleSort' :: [Int] -> [Int]
troubleSort' (x:y:z:xs) =
  if z < x
    then z : troubleSort' (y : x : xs)
    else x : troubleSort' (y : z : xs)
troubleSort' xs = xs

troubleSort :: [Int] -> [Int]
troubleSort [] = []
troubleSort xs =
  let t1 = troubleSort' xs
   in troubleSort (init t1) ++ [last t1]

tests :: TestTree
tests =
  testGroup
    ""
    [ testCase "OK" $ True @?= True
    , testCase "OK" $ findTrouble [8, 9, 10, 3, 2] @?= "2"
    , testCase "OK" $ findTrouble [8, 3] @?= "0"
    , testCase "OK" $ findTrouble [0, 1, 2, 3, 3] @?= "OK"
    , testCase "OK" $ solve 1 [6, 5, 6] @?= "0"
    , testCase "OK" $ solve 1 [5, 6, 5] @?= "1"
    , testCase "OK" $ solve 1 [5, 6, 5, 6] @?= "1"
    , testCase "OK" $ solve 1 [6, 5, 6, 7] @?= "0"
    , testCase "sample" $ do
        input <- readFile "./cases/2018QB/in"
        otput <- readFile "./cases/2018QB/out"
        runFile input @?= otput
    ]

runTests = defaultMain tests
