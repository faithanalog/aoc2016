module Days.Day4
  ( day4
  ) where

import Days.Prelude

part1 = ""
part2 = ""

day4 =
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
