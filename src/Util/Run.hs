module Util.Run where

import Util.Read (readLines, splitNLinePerCase)
import Util.Types (RunOne)

run :: FilePath -> RunOne -> IO ()
run filePath runOne = do
  file <- readLines filePath -- [String]
  let allLines = splitNLinePerCase 1 (drop 1 file) -- [[String]]
  let outputs = runOne <$> allLines
  print outputs
