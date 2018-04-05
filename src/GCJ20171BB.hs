module GCJ20171BB where

import Data.List (find, foldl', partition, permutations, sortOn)
import Data.Tuple.Curry (uncurryN)
import Debug.Trace
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = interact runFile

runFile :: String -> String
runFile = unlines . zipWith format [1 ..] . map (uncurryN solve) . parse

parse :: String -> [(Int, Int, Int, Int, Int, Int, Int)]
parse = parse' . map read . tail . words
  where
    parse' [] = []
    parse' (n:r:o:y:g:b:v:xs) = (n, r, o, y, g, b, v) : parse' xs

-- try uncurryN
-- uncurry7 :: (a -> b -> c -> d -> e -> f -> g -> h) -> (a, b, c, d, e, f, g) -> h
-- uncurry7 f' (a, b, c, d, e, f, g) = f' a b c d e f g
format :: Int -> String -> String
format x y = "Case #" ++ show x ++ ": " ++ y

solve :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> String
solve n r o y g b v =
  if hasDup stalls
    then "IMPOSSIBLE"
    else stalls
  where
    stalls = buildWith is js ks
    [is, js, ks] = reverse $ sortOn fst [(r, rs), (y, ys), (b, bs)]
    rs = replicate r 'R'
    ys = replicate y 'Y'
    bs = replicate b 'B'

buildWith :: (Int, String) -> (Int, String) -> (Int, String) -> String
buildWith (i, is) (j, js) (k, ks) =
  let ts = take j js ++ take (i - j) ks -- If I is the most, J is the second most, then first make a list of ITITITIT where T will first be J, then K if running out of J. To make a list of T, we need all the Js, and then as many Ks as possible, so that (len I) == (len ts)
      (fillingK, remainingK) = splitAt (i - j) ks -- fillingK is the Ks used in T, remainingK is the left over Ks that are not being used as T
      its = takeEach is ts -- make ITs with Is and Ts, so that it's a valid circle
   in insertRemainingK its remainingK

-- insert remaining K to after T
insertRemainingK :: Eq a => [a] -> [a] -> [a]
insertRemainingK ijk [] = ijk
insertRemainingK [] ks = ks
insertRemainingK (i:jk:ijk) (k:ks) = i : jk : k : insertRemainingK ijk ks

takeEach :: Eq a => [a] -> [a] -> [a]
takeEach xs [] = xs
takeEach [] ys = ys
takeEach (x:xs) (y:ys) = x : y : takeEach xs ys

hasDup :: String -> Bool
hasDup xs = or $ zipWith (==) xs $ drop 1 xs ++ take 1 xs

buildHs :: Int -> Int -> Int -> String
buildHs r y b = replicate r 'R' ++ replicate y 'Y' ++ replicate b 'B'

tests :: TestTree
tests =
  testGroup
    ""
    [ testCase "OK" $ True @?= True
    -- , testCase "hasDup" $ do
    --     hasDup "abc" @?= False
    --     hasDup "abb" @?= True
    --     hasDup "bbc" @?= True
    --     hasDup "cbc" @?= True
    , testCase "solve" $ do
        solve 3 1 0 1 0 1 0 @?= "BYR"
        solve 3 2 0 1 0 1 0 @?= "RBRY"
        solve 3 2 0 2 0 1 0 @?= "YRBYR"
        solve 3 2 0 2 0 2 0 @?= "BYRBYR"
        solve 6 2 0 2 0 2 0 @?= "BYRBYR"
        solve 3 1 0 2 0 0 0 @?= "IMPOSSIBLE"
        solve 7 3 0 3 0 1 0 @?= "YRBYRYR"
    , testCase "sample" $ do
        input <- readFile "./cases/20171BB/in"
        otput <- readFile "./cases/20171BB/out"
        runFile input @?= otput
    ]

runTests = defaultMain tests
