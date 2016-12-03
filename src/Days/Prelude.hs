module Days.Prelude
  ( Day(..)
  , module Text.Trifecta
  , module Data.List 
  , module Data.List.Split
  , module Control.Lens
  , module Control.Applicative
  , module Control.Monad.State
  , runDay
  ) where

import Text.Trifecta hiding (noneOf)
import Data.List hiding (uncons, span)
import Data.List.Split hiding (oneOf, endBy, sepBy)
import Control.Lens
import Control.Applicative
import Control.Monad.State


data Day a b c = Day
  { _parser :: Parser a
  , _dayPart1 :: a -> b
  , _dayPart2 :: a -> c
  }

runDay :: (Show b, Show c) => Day a b c -> String -> IO ()
runDay d f = do
  input <- parseFromFile (_parser d) f
  putStrLn "Part 1:"
  mapM_ (print . _dayPart1 d) input
  putStrLn "Part 2:"
  mapM_ (print . _dayPart2 d) input
