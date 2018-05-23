module GCJ2018R1A where

import Debug.Trace
import Test.Tasty
import Test.Tasty.HUnit

import Data.Array
import Data.List
import Data.Tuple

-- import Data.Set
main :: IO ()
main = interact runFile

runFile :: String -> String
runFile = unlines . zipWith format [1 ..] . map (uncurry5 solve) . parse

uncurry5 f (a, b, c, d, e) = f a b c d e

parse :: String -> [(Int, Int, Int, Int, String)]
parse = parse' . tail . words
  where
    parse' [] = []
    parse' (r:c:h:v:xs) =
      (_r, _c, _h, _v, concat (take _r xs)) : parse' (drop _r xs)
      where
        _r = read r
        _c = read c
        _h = read h
        _v = read v

format :: Int -> Bool -> String
format x y = "Case #" ++ show x ++ ": " ++ sf y
  where
    sf True = "POSSIBLE"
    sf False = "IMPOSSIBLE"

solve' :: Int -> Int -> Int -> Int -> Array (Int, Int) Int -> Bool
solve' r c h v xs
  | modr /= 0 || modc /= 0 = False
  | otherwise = isRowGood && isColGood && isCellGood
  where
    totalC = sum $ elems xs
    (nPerR, modr) = totalC `divMod` (h + 1)
    (nPerC, modc) = totalC `divMod` (v + 1)
    cells = (h + 1) * (v + 1)
    (nPerCell, _) = totalC `divMod` cells
    sumrowsxs = sum <$> rows xs
    sumcolsxs = sum <$> cols xs
    isRowGood = mulOf nPerR sumrowsxs
    isColGood = mulOf nPerC sumcolsxs
    firstRows = length $ takeWhile (<= nPerR) $ scanl1 (+) sumrowsxs
    firstCols = length $ takeWhile (<= nPerC) $ scanl1 (+) sumcolsxs
    isCellGood =
      nPerCell ==
      sum [xs ! (x, y) | x <- [1 .. firstRows], y <- [1 .. firstCols]]

rows :: Array (Int, Int) a -> [[a]]
rows rc = [[rc ! (r, c) | c <- [sc .. ec]] | r <- [sr .. er]]
  where
    ((sr, sc), (er, ec)) = bounds rc

cols :: Array (Int, Int) a -> [[a]]
cols rc = [[rc ! (r, c) | r <- [sr .. er]] | c <- [sc .. ec]]
  where
    ((sr, sc), (er, ec)) = bounds rc

mulOf :: Int -> [Int] -> Bool
mulOf nPerR rs = snd $ foldr iter (0, True) rs
  where
    iter r (total, True)
      | r + total > nPerR = (0, False)
      | r + total == nPerR = (0, True)
      | otherwise = (total + r, True)
    iter r (total, False) = (0, False)

solve :: Int -> Int -> Int -> Int -> String -> Bool
solve r c h v xs = solve' r c h v ls
  where
    xs' = fmap toBool xs
    ls = listArray ((1, 1), (r, c)) xs'

toBool :: Char -> Int
toBool '@' = 1
toBool '.' = 0

input :: IO String
input = readFile "./cases/2018R1A/in"

output :: IO String
output = readFile "./cases/2018R1A/out"

tests :: TestTree
tests =
  testGroup
    ""
    [ testCase "OK" $ True @?= True
    , testCase "OK" $ True @?= True
    , testCase "sample" $ do
        ipt <- input
        opt <- output
        runFile ipt @?= opt
    ]

rt = defaultMain tests
