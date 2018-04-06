module Hello where

main :: IO ()
main =
  print $
  length $ (+ 2) <$> (+ 2) <$> (+ 2) <$> (+ 2) <$> (+ 2) <$> [1 .. 100000000]
