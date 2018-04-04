module GCJ20171BA where

import Data.List (find, foldl', partition)
import Debug.Trace
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = interact runFile

runFile :: String -> String
runFile = unlines . zipWith format [1 ..] . map (uncurry3 solve) . parse

parse :: String -> [(Int, Int, [(Int, Int)])]
parse = parse' . tail . words
  where
    parse' [] = []
    parse' (n_:m_:xs) = (n, m, hs) : parse' rest
      where
        n = read n_
        m = read m_
        hs = parseHs rs
        (rs, rest) = splitAt (m * 2) xs

parseHs :: [String] -> [(Int, Int)]
parseHs [] = []
parseHs (x_:y_:xs) = (x, y) : parseHs xs
  where
    x = read x_
    y = read y_

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (r, c, rows) = f r c rows

format :: Int -> String -> String
format x y = "Case #" ++ show x ++ ": " ++ y

div' :: Int -> Int -> Float
div' a b = fromIntegral a / fromIntegral b

solve :: Int -> Int -> [(Int, Int)] -> String
solve d n hs = show $ fromIntegral d / maxHour
  where
    maxHour = maximum hours
    hours = timeToEnd d <$> hs
    timeToEnd d (start, speed) = (d - start) `div'` speed

tests :: TestTree
tests =
  testGroup
    ""
    [ testCase "sample" $ do
        input <- readFile "./cases/20171BA/in"
        otput <- readFile "./cases/20171BA/out"
        runFile input @?= otput
    ]

runTests = defaultMain tests
