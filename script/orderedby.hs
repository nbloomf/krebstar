#!/usr/bin/env stack
-- stack --resolver lts-12.21 script

import System.Environment

orderedBy :: (Eq a) => [a] -> [a] -> [a]
orderedBy as bs = foldl checkHead as bs
  where
    checkHead :: (Eq a) => [a] -> a -> [a]
    checkHead []     x = []
    checkHead (y:ys) x = if x == y then ys else y:ys

main :: IO ()
main = do
  args <- getArgs

  case args of
    ["--dict",path] -> do
      dict <- fmap lines $ readFile path
      words <- fmap lines getContents
      mapM_ putStrLn $ orderedBy words dict

    _ -> do
      putStrLn "usage: orderedby --dict PATH"
