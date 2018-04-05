module GCJ20171BB where

import Data.List (find, foldl', partition, permutations, sortOn)
import Debug.Trace
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = interact runFile

runFile :: String -> String
runFile = unlines . zipWith format [1 ..] . map (uncurry6 solve) . parse

parse :: String -> [(Int, Int, Int, Int, Int, Int, Int)]
parse = parse' . tail . words
  where
    parse' [] = []
    parse' (n_:r_:o_:y_:g_:b_:v_:xs) = (n, r, o, y, g, b, v) : parse' xs
      where
        n = read n_
        r = read r_
        o = read o_
        y = read y_
        g = read g_
        b = read b_
        v = read v_

uncurry6 :: (a -> b -> c -> d -> e -> f -> g -> h) -> (a, b, c, d, e, f, g) -> h
uncurry6 f' (a, b, c, d, e, f, g) = f' a b c d e f g

format :: Int -> String -> String
format x y = "Case #" ++ show x ++ ": " ++ y

solve :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> String
solve n r o y g b v =
  if hasDup stalls
    then "IMPOSSIBLE"
    else stalls
  where
    stalls = buildWith is js ks
    [(i, is), (j, js), (k, ks)] =
      reverse $ sortOn fst [(r, rs), (y, ys), (b, bs)]
    rs = replicate r 'R'
    ys = replicate y 'Y'
    bs = replicate b 'B'

buildWith :: String -> String -> String -> String
buildWith is js ks = takeEach is $ takeEach js ks

takeEach :: String -> String -> String
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
        solve 3 2 0 2 0 1 0 @?= "YRYBR"
        solve 3 2 0 2 0 2 0 @?= "BYBRYR"
        solve 6 2 0 2 0 2 0 @?= "BYBRYR"
        solve 3 1 0 2 0 0 0 @?= "IMPOSSIBLE"
    -- , testCase "sample" $ do
    --     input <- readFile "./cases/20171BB/in"
    --     otput <- readFile "./cases/20171BB/out"
    --     runFile input @?= otput
    ]

runTests = defaultMain tests
