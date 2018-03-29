module Util.Types where

-- RunOne takes x lines and return y lines as output
type RunOne = InputLines -> OutputLines

type InputLines = [String]

type OutputLines = [[String]]

data GCase = GCase
  { gcaseCount :: Int
  , gcaseLines :: [String]
  } deriving (Show, Eq, Ord)

type GCases = [GCase]
