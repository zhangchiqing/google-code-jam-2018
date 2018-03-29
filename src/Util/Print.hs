module Util.Print where

import Data.Foldable (traverse_)
import Util.Types (OutputLines)

printLines :: [String] -> Int -> IO ()
printLines [line] num = putStrLn $ "Case #" ++ show num ++ ": " ++ line
printLines ls num = do
  putStrLn $ "Case #" ++ show num
  traverse_ putStrLn ls

print :: OutputLines -> IO ()
print outputs = traverse_ (uncurry printLines) $ zip outputs [1 ..]

printMultiLine :: [String] -> Int -> IO ()
printMultiLine mlines num = do
  putStrLn $ "Case #" ++ show num ++ ":"
  traverse_ putStrLn mlines

printMulti :: [[String]] -> IO ()
printMulti outputs = traverse_ (uncurry printMultiLine) $ zip outputs [1 ..]
