module Main where

import Days.Prelude
import Days.Day1
import Days.Day2
import System.Environment

days :: [String -> IO ()]
days =
  [ runDay day1
  , runDay day2
  ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "Usage: aoc2016 <day> [test]"
    [x] -> do
      let n = read x
          run = days !! (n - 1)
      run ("input/day" ++ x)
    [x, "test"] -> do
      let n = read x
          run = days !! (n - 1)
      run ("input/day" ++ x ++ "test")
