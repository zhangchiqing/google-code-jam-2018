module GCJ2017QA where

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
runCase (caseNum, line) =
  let (faces, ks) = break (' ' ==) line
      k = read $ drop 1 ks
      output = solve 0 faces k
   in "Case #" ++ show caseNum ++ ": " ++ output

solve :: Int -> String -> Int -> String
solve count "" _ = show count
solve count ('+':faces) k = solve count faces k
solve count ('-':faces) k =
  if afterK' == "" && length firstK' < k'
    then "IMPOSSIBLE"
    else solve (count + 1) flipped k
  where
    k' = k - 1
    (firstK', afterK') = splitAt k' faces
    flipped = flipN firstK' <> afterK'

flipOne :: Char -> Char
flipOne '-' = '+'
flipOne '+' = '-'

flipN :: String -> String
flipN xs = flipOne <$> xs

tests :: TestTree
tests =
  testGroup
    ""
    [ testCase "0" $ runCase (0, "-+-+ 2") @?= "Case #0: 2"
    , testCase "1" $ runCase (0, "---+ 3") @?= "Case #0: 1"
    , testCase "2" $ runCase (0, "---+-++- 3") @?= "Case #0: 3"
    , testCase "3" $ runCase (0, "+++++ 4") @?= "Case #0: 0"
    , testCase "4" $ runCase (0, "-+-+- 4") @?= "Case #0: IMPOSSIBLE"
    , testCase "sample" $ do
        input <- readFile "./cases/2017QA/in"
        otput <- readFile "./cases/2017QA/out"
        runFile input @?= otput
    ]
