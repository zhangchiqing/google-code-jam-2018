module GCJ2017QA where

import Data.Foldable (traverse_)
import Data.List (partition)

main :: IO ()
main = run "/Users/leo/Downloads/A-small-practice (1).in"

run :: FilePath -> IO ()
run filePath = do
  file <- lines <$> readFile filePath
  let cases = drop 1 file
      outputs = runCase <$> zip [1 ..] cases
  traverse_ putStrLn outputs
  return ()

runCase :: (Int, String) -> String
runCase (caseNum, line) =
  let (faces, ks) = partition (' ' ==) line
      k = read ks
      output = solve faces k
   in "Case #" ++ show caseNum ++ ": " ++ output

solve :: String -> Int -> String
solve _ _ = "IMPOSSIBLE"
