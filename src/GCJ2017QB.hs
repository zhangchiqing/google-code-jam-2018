module GCJ2017QB where

import Data.Char (intToDigit)
import Data.Foldable (traverse_)
import Data.Monoid ((<>))
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = interact runFile

runFile :: String -> String
runFile file = unlines $ runCase <$> cases
  where
    cases = zip [1 ..] $ drop 1 $ lines file

runCase :: (Int, String) -> String
runCase (caseNum, line) = "Case #" ++ show caseNum ++ ": " ++ solve line

solve :: String -> String
solve xs = dropWhile (== '0') $ foldr solveXs "" xs

solveXs :: Char -> String -> String
solveXs x "" = [x]
solveXs x xs
  | x <= h = x : xs
  | x > h = decrease x : all9 xs
  where
    h = head xs

decrease :: Char -> Char
decrease x = intToDigit ((charToInt x) - 1)

charToInt :: Char -> Int
charToInt c = read $ c : ""

all9 :: String -> String
all9 = fmap (const '9')

tests :: TestTree
tests =
  testGroup
    ""
    [ testCase "0" $ runCase (0, "12") @?= "Case #0: 12"
    , testCase "sample" $ do
        input <- readFile "./cases/2017QB/in"
        otput <- readFile "./cases/2017QB/out"
        runFile input @?= otput
    ]
