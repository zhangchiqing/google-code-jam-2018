module GCJ2018QDD where

import Data.List

main :: IO ()
main = interact runFile

runFile :: String -> String
runFile = unlines . zipWith format [1 ..] . map solve . parse

parse :: String -> [Double]
parse = map read . drop 1 . words

type Point = (Double, Double)

type Point3 = (Double, Double, Double)

format :: Int -> (Point3, Point3, Point3) -> String
format i ((ix, iy, iz), (jx, jy, jz), (kx, ky, kz)) =
  unlines
    [ "Case #" ++ show i ++ ":"
    , show ix ++ " " ++ show iy ++ " " ++ show iz
    , show jx ++ " " ++ show jy ++ " " ++ show jz
    , show kx ++ " " ++ show ky ++ " " ++ show kz
    ]

solve :: Double -> (Point3, Point3, Point3)
solve a =
  if a <= sqrt 2
    then solve2D a
    else solve3D a solved2D

solved2D = solve2D (sqrt 2)

solve2D :: Double -> (Point3, Point3, Point3)
solve2D a = ((ix, iy, 0.0), (jx, jy, 0.0), (0.0, 0.0, 0.5))
  where
    b = findB a
    (ix, iy) = iFromB b
    (jx, jy) = jFromB b

solve3D :: Double -> (Point3, Point3, Point3) -> (Point3, Point3, Point3)
solve3D a = rotate3D (findT a)

rotate3D :: Double -> (Point3, Point3, Point3) -> (Point3, Point3, Point3)
rotate3D t ((ix, iy, iz), (jx, jy, jz), (kx, ky, kz)) =
  ((ix, iy', iz'), (jx, jy', jz'), (kx, ky', kz'))
  where
    iy' = iy * cost
    iz' = iy * sint
    jy' = jy * cost
    jz' = jy * sint
    ky' = -kz * sint
    kz' = kz * cost
    cost = cos t
    sint = sin t

magicN = 0.6155555555

-- a = sin t + sqrt 2 * cos t
findT :: Double -> Double
findT a = binarySearch tToA a 0 0 magicN

tToA :: Double -> Double
tToA t = sin t + sqrt 2 * cos t

-- I = (cosB * 0.5, sinB * 0.5)
iFromB :: Double -> Point
iFromB b = (cos b * 0.5, sin b * 0.5)

-- J = (cos(pi/2 + B) * 0.5, sin(pi/2 + B) * 0.5)
jFromB :: Double -> Point
jFromB b = (cos d * 0.5, sin d * 0.5)
  where
    d = pi / 2 + b

findB :: Double -> Double
findB a = binarySearch bToA a 0 0 (pi / 4)

-- A = sinB + cosB
bToA :: Double -> Double
bToA b = sin b + cos b

maxA :: Double
maxA = 1.732050

binarySearch ::
     (Double -> Double) -> Double -> Double -> Double -> Double -> Double
binarySearch f a mid min max =
  if (abs (aMid - a)) < 0.000000001
    then mid
    else if aMid > a
           then binarySearch f a ((min + mid) / 2) min mid
           else binarySearch f a ((mid + max) / 2) mid max
  where
    aMin = f min
    aMid = f mid
    aMax = f max
