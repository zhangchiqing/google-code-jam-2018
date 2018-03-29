module Util.Read where

import Data.List.Split (chunksOf)

readLines :: FilePath -> IO [String]
readLines fileName = lines <$> readFile fileName

splitNLinePerCase :: Int -> [String] -> [[String]]
splitNLinePerCase = chunksOf
