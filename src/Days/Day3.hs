module Days.Day3
  ( day3
  ) where

import Days.Prelude

type Tri = [Integer]

valid [a, b, c] = a + b > c && a + c > b && b + c > a

part1 = length . filter valid
part2 = part1 . concatMap transpose . chunksOf 3

day3 =
  Day
  { _parser = parser
  , _dayPart1 = part1
  , _dayPart2 = part2
  }
  where
    parser :: Parser [Tri]
    parser = do
      let tri = do
            a <- natural
            b <- natural
            c <- natural
            pure [a, b, c]
      whiteSpace >> some tri
