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

format :: Int -> String -> String
format x y = "Case #" ++ show x ++ ": " ++ y

data L
  = R
  | Y
  | B
  | O Int
  | G Int
  | V Int
  deriving (Show)

solve :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> String
solve n r o y g b v
  | n == o + b && o == b = takeEach (replicate o 'O') (replicate b 'B')
  | n == g + r && g == r = takeEach (replicate g 'G') (replicate r 'R')
  | n == v + y && v == y = takeEach (replicate v 'V') (replicate y 'Y')
  | g > 0 && r < g + 1 = "IMPOSSIBLE"
  | v > 0 && y < v + 1 = "IMPOSSIBLE"
  | o > 0 && b < o + 1 = "IMPOSSIBLE"
  | otherwise =
    if stallHasDup stalls
      -- then renderStalls stalls
      then "IMPOSSIBLE"
      else renderStalls stalls
  where
    stalls = solve' (length rs', rs') (length ys', ys') (length bs', bs')
    rs' = rs ++ gs
    ys' = ys ++ vs
    bs' = bs ++ os
    rs = replicate (wrapNeed r g) R
    os = [O o | o > 0]
    ys = replicate (wrapNeed y v) Y
    gs = [G g | g > 0]
    bs = replicate (wrapNeed b o) B
    vs = [V v | v > 0]

wrapNeed :: Int -> Int -> Int
wrapNeed first 0 = first
wrapNeed first second = first - (second + 1)

solve' :: Show a => (Int, [a]) -> (Int, [a]) -> (Int, [a]) -> [a]
solve' (r, rs) (y, ys) (b, bs) = buildWith iis jjs kks
  where
    [iis, jjs, kks] = reverse $ sortOn fst [(r, rs), (y, ys), (b, bs)]

buildWith :: Show a => (Int, [a]) -> (Int, [a]) -> (Int, [a]) -> [a]
buildWith (i, is) (j, js) (k, ks) =
  let ts = take j js ++ fillingK -- If I is the most, J is the second most, then first make a list of ITITITIT where T will first be J, then K if running out of J. To make a list of T, we need all the Js, and then as many Ks as possible, so that (len I) == (len ts)
      (fillingK, remainingK) = splitAt (i - j) ks -- fillingK is the Ks used in T, remainingK is the left over Ks that are not being used as T
      its = takeEach is ts -- make ITs with Is and Ts, so that it's a valid circle
   in insertRemainingK its remainingK

-- insert remaining K to after T
insertRemainingK :: Show a => [a] -> [a] -> [a]
insertRemainingK ijk [] = ijk
insertRemainingK [] ks = ks
insertRemainingK (i:jk:ijk) (k:ks) = i : jk : k : insertRemainingK ijk ks

-- insertRemainingK [i] [k] = [i, k]
takeEach :: [a] -> [a] -> [a]
takeEach xs [] = xs
takeEach [] ys = ys
takeEach (x:xs) (y:ys) = x : y : takeEach xs ys

renderStalls :: [L] -> String
renderStalls = concatMap renderStall

renderStall :: L -> String
renderStall R = "R"
renderStall Y = "Y"
renderStall B = "B"
renderStall (O o) = interfill 'O' 'B' o
renderStall (G g) = interfill 'G' 'R' g
renderStall (V v) = interfill 'V' 'Y' v

interfill :: Char -> Char -> Int -> String
interfill l f n = takeEach (replicate (n + 1) f) (replicate n l)

stallHasDup :: [L] -> Bool
stallHasDup ls = or $ zipWith lEq ls $ drop 1 ls ++ take 1 ls

lEq :: L -> L -> Bool
lEq R R = True
lEq Y Y = True
lEq B B = True
lEq R (G _) = True
lEq Y (V _) = True
lEq B (O _) = True
lEq _ _ = False

hasDup :: String -> Bool
hasDup xs = or $ zipWith (==) xs $ drop 1 xs ++ take 1 xs

tests :: TestTree
tests =
  testGroup
    ""
    [ testCase "OK" $ True @?= True
    , testCase "hasDup" $ hasDup "abc" @?= False
    , testCase "hasDup" $ hasDup "abb" @?= True
    , testCase "hasDup" $ hasDup "bbc" @?= True
    , testCase "hasDup" $ hasDup "cbc" @?= True
    , testCase "interfill" $ interfill 'O' 'B' 3 @?= "BOBOBOB"
    , testCase "interfill" $ interfill 'O' 'B' 1 @?= "BOB"
    , testCase "interfill" $ interfill 'O' 'B' 0 @?= "B"
                           ---   R O Y G B V
    , testCase "solve" $ solve 3 1 0 1 0 1 0 @?= "BYR"
    , testCase "solve" $ solve 3 2 0 1 0 1 0 @?= "RBRY"
    , testCase "solve" $ solve 3 2 0 2 0 1 0 @?= "YRBYR"
    , testCase "solve" $ solve 3 2 0 2 0 2 0 @?= "BYRBYR"
    , testCase "solve" $ solve 6 2 0 2 0 2 0 @?= "BYRBYR"
    , testCase "solve" $ solve 4 1 0 1 0 2 0 @?= "BYBR"
    , testCase "solve" $ solve 6 1 0 2 0 2 1 @?= "BYVYBR"
    , testCase "solve" $ solve 3 1 0 2 0 0 0 @?= "IMPOSSIBLE"
    , testCase "solve" $ solve 13 3 1 3 1 3 2 @?= "BRYVYVYBOBRGR"
    , testCase "solve" $ solve 7 3 0 3 0 1 0 @?= "YRBYRYR"
    , testCase "sample" $ do
        input <- readFile "./cases/20171BB/in"
        otput <- readFile "./cases/20171BB/out"
        runFile input @?= otput
    ]

runTests = defaultMain tests
