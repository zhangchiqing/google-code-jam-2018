module GCJ2017QC where

import qualified Data.IntMap.Strict as M
import Debug.Trace
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = interact runFile

runFile :: String -> String
runFile = unlines . zipWith format [1 ..] . map (uncurry3 solve) . parse

parse :: String -> [(Int, Int, [String])]
parse = parse' . tail . words
  where
    parse' [] = []
    parse' (n_:m_:xs) = (n, m, rs) : parse' rest
      where
        n = read n_
        m = read m_
        (rs, rest) = splitAt n xs

uncurry3 f (a, b, c) = f a b c

format :: Int -> [String] -> String
format x rows = unlines $ ("Case #" ++ show x ++ ": ") : rows

solve :: Int -> Int -> [String] -> [String]
solve a b c = [show (a, b, c)]

tests :: TestTree
tests =
  testGroup
    ""
    [ testCase "sample" $ do
        input <- readFile "./cases/20171A/in"
        otput <- readFile "./cases/20171A/out"
        runFile input @?= otput
    ]

runTests = defaultMain tests
