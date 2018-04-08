module GCJ2018QA where

import Debug.Trace
import Test.Tasty
import Test.Tasty.HUnit

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

computeD :: String -> Int
computeD xs = iter xs (0, 1)
  where
    iter [] (total, current) = total
    iter ('S':xs) (total, current) = iter xs (total + current, current)
    iter ('C':xs) (total, current) = iter xs (total, current * 2)

-- CS -> SC
findAndSwapFarRightCSToSC :: Int -> String -> Maybe String
findAndSwapFarRightCSToSC n p =
  if null cIndexes
    then Nothing
    else Just $ swapCAtIndex (last cIndexes) p
  where
    cIndexes = filter (good n) $ elemIndices 'C' p
    good n index = index < n - 1

swapCAtIndex :: Int -> String -> String
swapCAtIndex index p = first ++ "SC" ++ drop 2 sLast
  where
    (first, sLast) = splitAt index p

tests :: TestTree
tests =
  testGroup
    ""
    [ testCase "OK" $ True @?= True
    , testCase "sample" $ do
        input <- readFile "./cases/2018QA/in"
        otput <- readFile "./cases/2018QA/out"
        runFile input @?= otput
    ]

runTests = defaultMain tests
