module GCJ20171A where

import Data.List (find, foldl', partition)
import Debug.Trace
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = interact runFile

runFile :: String -> String
runFile =
  unlines . concat . zipWith format [1 ..] . map (uncurry3 solve) . parse

parse :: String -> [(Int, Int, [String])]
parse = parse' . tail . words
  where
    parse' [] = []
    parse' (n_:m_:xs) = (n, m, rs) : parse' rest
      where
        n = read n_
        m = read m_
        (rs, rest) = splitAt n xs

uncurry3 ::
     (Int -> Int -> [String] -> [String]) -> (Int, Int, [String]) -> [String]
uncurry3 f (r, c, rows) = f r c rows

format :: Int -> [String] -> [String]
format x y = ("Case #" ++ show x ++ ":") : y

solve :: Int -> Int -> [String] -> [String]
solve n m rows = headFilled ++ [firstFilled] ++ restFilled
  where
    (emptyLines, nonEmptyLines) = span lineHasNoLetter rows -- ([String], [String])
    firstLine =
      if null nonEmptyLines
        then trace ("empty lines" ++ show rows) ""
        else head nonEmptyLines
    restLines = drop 1 nonEmptyLines
    firstFilled = fillLine firstLine -- String
    headFilled = fillEmptyLines firstFilled emptyLines -- [String]
    restFilled = fillRestLines firstFilled restLines -- [String]

lineHasNoLetter :: String -> Bool
lineHasNoLetter = all (== '?')

fillLine :: String -> String
fillLine xs =
  case maybeLast of
    Just last -> snd $ foldr filling (last, "") xs
    Nothing -> trace ("fillLine should have letter: " ++ xs) xs
  where
    maybeLast = find (/= '?') $ reverse xs

filling :: Char -> (Char, String) -> (Char, String)
filling '?' (last, xs) = (last, last : xs)
filling x (last, xs) = (x, x : xs)

fillEmptyLines :: String -> [String] -> [String]
fillEmptyLines line xs = replicate (length xs) line

fillRestLines :: String -> [String] -> [String]
fillRestLines line xs = reverse $ fst $ foldl' fillingR ([], line) xs
  where
    fillingR (lines, last) line
      | isEmpty = (last : lines, last)
      | otherwise = (filled : lines, filled)
      where
        isEmpty = lineHasNoLetter line
        filled = fillLine line

tests :: TestTree
tests =
  testGroup
    ""
    [ testCase "unit" $ do
        fillLine "?C?" @?= "CCC"
        fillLine "?CC" @?= "CCC"
        fillLine "CCC" @?= "CCC"
    , testCase "sample" $ do
        input <- readFile "./cases/20171A/in"
        otput <- readFile "./cases/20171A/out"
        runFile input @?= otput
    ]

runTests = defaultMain tests
