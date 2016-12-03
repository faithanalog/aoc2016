{-# LANGUAGE TemplateHaskell #-}
module Days.Prelude
  ( Day(..)
  , module Text.Trifecta
  , module Data.List 
  , module Control.Lens
  , module Control.Applicative
  , module Control.Monad.State
  , runDay
  ) where

import Text.Trifecta hiding (noneOf)
import Data.List hiding (uncons, span)
import Control.Lens
import Control.Applicative
import Control.Monad.State


data Day a b c = Day
  { _parser :: Parser a
  , _dayPart1 :: a -> b
  , _dayPart2 :: a -> c
  }

makeLenses ''Day

runDay :: (Show b, Show c) => Day a b c -> String -> IO ()
runDay d f = do
  input <- parseFromFile (d ^. parser) f
  putStrLn "Part 1:"
  mapM_ (print . (d ^. dayPart1)) input
  putStrLn "Part 2:"
  mapM_ (print . (d ^. dayPart2)) input
