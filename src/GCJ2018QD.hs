module GCJ2018QD where

import Debug.Trace
import Test.Tasty
import Test.Tasty.HUnit

import Data.List

main :: IO ()
main = interact runFile

runFile :: String -> String
runFile = unlines . zipWith format [1 ..] . map solve . parse

parse :: String -> [Float]
parse = map read . drop 1 . words

type Point = (Float, Float)

format :: Int -> (Point, Point) -> String
format i ((ix, iy), (jx, jy)) =
  unlines
    [ "Case #" ++ show i ++ ":"
    , show ix ++ " " ++ show iy ++ " " ++ show z
    , show jx ++ " " ++ show jy ++ " " ++ show z
    , "0 0 0.5"
    ]
  where
    z = 0

solve :: Float -> (Point, Point)
solve a =
  let b = findB a
      i = iFromB b
      j = jFromB b
   in (i, j)

-- I = (cosB * 0.5, sinB * 0.5)
iFromB :: Float -> Point
iFromB b = (cos b * 0.5, sin b * 0.5)

-- J = (cos(pi/2 + B) * 0.5, sin(pi/2 + B) * 0.5)
jFromB :: Float -> Point
jFromB b = (cos d * 0.5, sin d * 0.5)
  where
    d = pi / 2 + b

findB :: Float -> Float
findB a = iter a 0 0 (pi / 4)

-- A = sinB + cosB
bToA b = sin b + cos b

iter a mid min max =
  if aMid == a
    then mid
    else if aMid > a
           then iter a ((min + mid) / 2) min mid
           else iter a ((mid + max) / 2) mid max
  where
    aMin = bToA min
    aMid = bToA mid
    aMax = bToA max

tests :: TestTree
tests =
  testGroup
    ""
    [ testCase "OK" $ True @?= True
    , testCase "OK" $ findB 1 @?= 0
    , testCase "OK" $ findB (sqrt 2) @?= pi / 4
    -- , testCase "sample" $ do
    --     input <- readFile "./cases/2018QD/in"
    --     otput <- readFile "./cases/2018QD/out"
    --     runFile input @?= otput
    ]

runTests = defaultMain tests
